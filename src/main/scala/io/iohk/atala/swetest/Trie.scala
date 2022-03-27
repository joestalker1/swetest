package io.iohk.atala.swetest


import scala.annotation.tailrec
import scala.collection.mutable.Map

sealed trait TrieNode

case class Node(data: scala.collection.mutable.Map[Byte, TrieNode] = Map.empty[Byte, TrieNode]) extends TrieNode

case class LeafNode(index: Int) extends TrieNode

/**
 * Store array byte in trie. Last node contains block index.
 */

case class Trie(root: Node = Node()) {

  private def updateChildLeafNodeToNode(hash: Byte, parent: Node, leafNode: TrieNode): Unit = {
    leafNode match {
      case leaf@LeafNode(_) => val newNode = Node()
        newNode.data(hash) = leaf
        parent.data(hash) = newNode
      case _ =>
    }
  }

  private def createException(hashArray: Array[Byte], blockIndex: Int): RuntimeException = {
    new RuntimeException(s"Expect Trie.Node not LeafNode for [$hashArray, $blockIndex]")
  }

  /**
   * Append byte array and last byte points leaf node with block index
   *
   * @param hashArray
   * @param blockIndex
   * @return
   */
  def append(hashArray: Array[Byte], blockIndex: Int): Either[Throwable, Unit] = {

    def append(index: Int, current: TrieNode): Either[Throwable, Unit] = {
      if (index == hashArray.length) {
        //update node to last byte points out a leaf nodb with block index.
        current match {
          case node@Node(_) => val lastByte = node.data.get(hashArray.last)
            lastByte.fold(node.data(hashArray.last) = LeafNode(blockIndex)) { nd =>
              updateChildLeafNodeToNode(hashArray.last, node, nd)
              node.data(hashArray.last) = LeafNode(blockIndex)
            }
            Right(())
          case _ => Left(createException(hashArray, blockIndex))
        }
      } else {
        val hash = hashArray(index)
        current match {
          case node@Node(_) => val child = node.data.get(hash)
            child.fold(node.data(hash) = Node())(updateChildLeafNodeToNode(hash, node, _))
          case _ => //case can't be because we fix the leaf in advance
        }
        //go to the next node
        current match {
          case node@Node(_) => append(index + 1, node.data(hash))
          case _ => Left(createException(hashArray, blockIndex))
        }
      }
    }

    append(0, root)
  }

  /**
   * Find a leaf node with block index for the given byte array
   *
   * @param hashArray
   * @return
   */
  def findIndex(hashArray: Array[Byte]): Option[Int] = {

    def findIndex(hashIndex: Int, current: TrieNode): Option[Int] = {
      if (hashIndex == hashArray.length - 1) {
        current match {
          case node@Node(_) => val child = node.data.get(hashArray(hashIndex))
            child.fold(None: Option[Int]) { trieNode =>
              // we have last part of hash
              trieNode match {
                case LeafNode(index) => Option(index)
                // if we have chain data(hash) -> Node(data(hash)) then data(hash)
                // can only contain LeafNode
                case Node(data) => data.get(hashArray(hashIndex)).map(n => n.asInstanceOf[LeafNode].index)
              }
            }
          case _ => None
        }
      } else {
        //go to next node if possible
        current match {
          case Node(data) => val nextNode = data.get(hashArray(hashIndex))
            nextNode.fold(None: Option[Int])(findIndex(hashIndex + 1, _))
          case _ => None
        }
      }
    }

    findIndex(0, root)
  }
}
