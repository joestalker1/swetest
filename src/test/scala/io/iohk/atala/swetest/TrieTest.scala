package io.iohk.atala.swetest

import org.scalatest._
import flatspec._
import matchers._

class TrieTest extends AnyFlatSpec with should.Matchers {

  "A trie" should "append hash array successfully" in {
    val trie = Trie()
    val hashArray1 = Array[Byte](1, 2, 3, 4, 5, 6)
    val hashArray2 = Array[Byte](1, 2, 3)
    trie.append(hashArray1, 1) should be(Right(()))
    trie.append(hashArray2, 1) should be(Right(()))
  }

  "A trie" should "append few hash arrays and find them" in {
    val trie = Trie()
    //2 hash array with common prefix
    val hashArray1 = Array[Byte](1, 2, 3, 4, 5, 6)
    val blockIndex1 = 1
    val hashArray2 = Array[Byte](1, 2, 3, 4)
    val blockIndex2 = 2
    val hashArray3 = Array[Byte](10, 12, 13, 14)
    val blockIndex3 = 2
    trie.append(hashArray1, blockIndex1) should be(Right())
    trie.append(hashArray2, blockIndex2) should be(Right())
    trie.append(hashArray3, blockIndex3) should be(Right())
    //test find
    trie.findIndex(hashArray1) should be(Option(blockIndex1))
    trie.findIndex(hashArray2) should be(Option(blockIndex2))
    trie.findIndex(hashArray3) should be(Option(blockIndex3))
    //not existing hash
    trie.findIndex(Array[Byte](1,2,3)) should be(None)
  }
}
