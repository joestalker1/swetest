package io.iohk.atala.swetest

import java.security.MessageDigest
import io.iohk.atala.swetest.Base._

import scala.annotation.tailrec
import cats.implicits._
import cats._

import java.util.concurrent.locks.ReentrantReadWriteLock
///////////////////////////////////////////////////////////////////////
// "mini-chain" SWE coding challenge in Scala, version 5
//
// COPYRIGHT © IOHK
///////////////////////////////////////////////////////////////////////
//
// We will be using this to have a technical discussion.
// A few focused hours should be enough but ultimately it is up to you.
// We definitely do not want you to spend many days of your personal
// time.
//
// Remember, this is a representation of your skill and ultimately the
// first point of contact with our technical recruitment team.
// If you think that something is worth doing - it's probably
// a good idea to show it off (eg. error handling, sensible unit test coverage, formatting).
//
// You can think about this code as virtual collaboration with a
// colleague.
//
// General requirements are:
//
//  - Your submission is an SBT project.
// 
//  - Your submission is a git repository with a commit log that you would submit as a PR.
//
//  - Ultimately it is up to the reviewer to understand the code submitted.
//    They do their best but if the code is hard to follow 
//    that will affect the evaluation.
//
//  - Review and complete the design and implementation.
//
//  - The coding language is Scala.
//
//  - You are free to come up with any solution you want but take into account
//    memory and time complexity.
//
//  - Comments stating "this could be done better with 'XYZ'" will be ignored.
//    We can speculate all day but at the end the code submitted is what is evaluated.
//
//  - If you come up with a solution for a piece of the code, 
//    but it is inefficient/doesn't capture all scenarios
//    and it would be very time consuming to implement a better one,
//    please add a comment describing the improvement, in what way it is better
//    and why you think it would be a lot of work.
//
//  - You must devise some tests to cover the code 
//    that would support its maintenance over time and in a team setting.
//    hint: this does not mean one test that shows one happy path engaging the whole application.
//
//  - Feel free to change any part of the design.
//
//  - The code is written in an OO imperative style. We strongly encourage an FP oriented solution.
//
//  - You are free to add any external libraries you need.
//
//  - You need to document the system requirements of the project (eg. major jvm version)
//    in a file called requirements.md.
//
//  - You must share your submission privately (i.e. not a public GitHub
//    repository). Typically, a zip archive with the local git repo project source code.
//
//  - Assume the code will run in a multi-threaded, multi-cpu environment.
//    You never know how concurrency (or is it parallelism?) will kick in.
//
//  - Whenever you are making changes to the supplied code base make a note with a reason/purpose 
//    (perhaps as part of the commit message).
//
///////////////////////////////////////////////////////////////////////
// We build some ingredients of a mini blockchain. Concepts we will
// be dealing with are:
//
//   - Hash, which is the result of a cryptographic hash computation.
//     We use SHA-256.
//
//   - Transaction, which is the representation of data stored in the
//     "mini-chain". Currently, just a string message.
//
//   - Block, the fundamental building block, which we use to build
//     the chain, and holds many transactions.
//
//   - Blockchain, a singly-linked list of Blocks with some extra
//     API to build and query it.
//
//   - Proof-of-Work or PoW for short.
//     A computationally intensive algorithm whose role is to solve a
//     puzzle that allows the next block to appear in the blockchain.
//     We "mine" the next block by using PoW.
//
///////////////////////////////////////////////////////////////////////

// Some base definitions
object Base {
  // When you see Unknown, replace it with appropriate type on a per
  // case basis. So if you see:
  //
  //   def append(block: Block): Unknown
  //
  // and later in the code:
  //
  //   def findByIndex(index: Int): Unknown
  //
  // and we ask you to implement the methods, you do not necessarily
  // have to replace Unknown with the same type in both cases.
  type Unknown = Nothing

  type Nonce = Long

  type Bytes = Array[Byte]
  val Bytes = new Array[Byte](_: Int)

  type Number = BigInt
  val Number = BigInt

  def toHexString(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")

}


// The idea behind any cryptographic hash representation in "mini-chain"
// is to treat it as an immutable array of bytes that can be also viewed
// as a number or a hex string. You will see that the number representation
// is used in the mining process. The hex representation is for logging
// purposes.
case class Hash(bytes: Bytes) {
  def toNumber: Number = Number(1, bytes)

  def toHexString: String = Base.toHexString(bytes)
}


object Hash {
  def fromBytes(xs: Byte*): Hash = Hash(xs.toArray)
}

// Hashes are produced by a cryptographic function and in "mini-chain" we
// use SHA-256, which always generates a 32-byte (256-bit) value.
object Sha256 {
  val NumberOfBytes = 32
  //use theadlocal because TheDigest is not threadsafe and may be thr bottleneck in multithreaded env, so naive approach it
  // has digest for every thread from pool
  val TheDigest = new ThreadLocal[MessageDigest] {
    override def initialValue = MessageDigest.getInstance("SHA-256")
  }


  // We pre-compute the hash of an empty array of 32 bytes.
  // We call this the "Zero_Hash".
  val Zero_Hash: Hash = Hash(Bytes(NumberOfBytes))

  // We use this to hash a composite structure whose constituents can be given
  // as byte arrays. We just feed everything to SHA-256.
  def apply(bytes: Bytes*): Hash = {
    for (bytes <- bytes) {
      TheDigest.get().update(bytes)
    }

    val hash = TheDigest.get().digest()
    assert(hash.length == NumberOfBytes)
    Hash(hash)
  }
}

// In "mini-chain", a transaction is just a message.
case class Transaction(data: String)

object Transaction {
  implicit def strToTrans(s: String): Transaction = Transaction(s)

  def hashOf(transaction: Transaction): Hash = Hash(transaction.data.getBytes)
}

// Now we are ready to describe the Block.
// Every block has an index, starting from zero (0).
// The block at index 0 is called the Genesis block.
// A block links back to the previous (parent) block.
// Of course, we also record the transactions that this block introduces to our mini-chain.
// We'll see the meaning of the other fields as we move along.
case class Block(
                  index: Int,
                  parentHash: Hash,
                  transactions: Seq[Transaction],
                  miningTargetNumber: Number,
                  nonce: Nonce,
                ) {

  // To get the crypto hash of the block, just feed all fields to SHA-256.
  val cryptoHash: Hash = {
    //index to byte array
    val indexBytes = Number(index).toByteArray
    val parHashBytes = parentHash.bytes
    val miningTargetNumberBytes = miningTargetNumber.toByteArray
    val nonceBytes = Number(nonce).toByteArray
    Sha256.apply(indexBytes, parHashBytes, transactions.map(_.data.getBytes).flatten.toArray, miningTargetNumberBytes, nonceBytes)
  }

  // The essence of PoW is that it is a problem whose solution is easy
  // (in computational resources) to verify but difficult to find.
  def verifyThisHasBeenMinedProperly(): Unit =
    assert(isThisHasBeenMinedProperly())

  def isThisHasBeenMinedProperly(): Boolean =
    cryptoHash.toNumber < miningTargetNumber
}

object Miner {
  // NOTE: A Hash is also a Number, we use the two interchangeably.
  //
  // Mining is about computing hashes until we get something that is less
  // than a given target number.
  // This target serves, in a way, as the maximum possible number that a
  // proof of work computation should produce.
  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  // Whoa! We actually mine the Genesis block.
  // Normally, this is done by the system during bootstrapping
  // and every other block is mined by a miner.
  final val Genesis = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Sha256.Zero_Hash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    StdMiningTargetNumber,
  )

  // We basically create a target number with the requirement of having
  // some leading zeros. More leading zeros means smaller target number.
  //
  // NOTE: To actually solve the current coding challenge, would you choose a
  // small or a big number of leading zeros?
  def targetByLeadingZeros(zeros: Int): BigInt = {
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        }
        else {
          0xff.toByte
        }
      }

    BigInt(1, bytes)
  }

  // And now let's implement the actual "proof-of-work"-style computation.
  // Compare the parameters of this method with the fields of a Block and
  // you'll see that the only thing missing here is the nonce. Here is why.
  //
  // Initially we have all the fixed elements a block:
  //
  //  - index,
  //  - parentHash,
  //  - transactions,
  //  - miningTargetNumber
  //
  // and by varying the nonce we try to have a block hash that is below the
  // given miningTargetNumber.
  //
  // NOTE Remember that the block hash can be transformed to an actual number,
  //      so we can talk about hash and number interchangeably.
  def mineNextBlock(
                     index: Int,
                     parentHash: Hash,
                     transactions: Seq[Transaction],
                     miningTargetNumber: BigInt,
                   ): Block = {
    // Solve this informal inequality for nonce:
    //
    //   Hash(block; nonce).toNumber < miningTargetNumber
    //
    // where Hash(block; nonce) is a function of nonce only, all the other block
    // field values are just the given method arguments.
    @tailrec
    def findNonce(curNonce: Long, go: Boolean): Long = {
      if (!go) curNonce
      else {
        val block = Block(index, parentHash, transactions, miningTargetNumber, curNonce)
        val validBlock = block.isThisHasBeenMinedProperly()
        //println(s"try ${block.cryptoHash.toHexString} is $validBlock")
        val nextNonce = if (validBlock) curNonce else curNonce + 1
        findNonce(nextNonce, !validBlock)
      }
    }

    //start nonce is Long min value
    val nonce = findNonce(Long.MinValue, true)
    Block(index, parentHash, transactions, miningTargetNumber, nonce)
  }
}

// A Blockchain is a sequence of blocks, each one having an index.
// The index of a block is the index of its parent plus one.
// A Blockchain always has a genesis block at index 0, which is the lowest index.
sealed trait Blockchain {
  // Add a block to the chain.
  // The return type is up to you, as explained in the definition of Unknown.
  def append(block: Block): Either[Throwable, Block]

  // Find a block by index.
  def findByIndex(index: Int): Option[Block]

  // Find a block by hash.
  def findByHash(hash: Hash): Option[Block]

  // Find a common ancestor between this blockchain and that blockchain.
  def common_ancestor(that: Blockchain): Option[Hash]
}

// Implement an in-memory blockchain that internally has an indexing data structure.
// The purpose of this internal data structure is to avoid traversing the linked list
// of blocks when answering queries like findByIndex.

class FastBlockchain extends Blockchain {
  private val indexToBlock = scala.collection.mutable.Map.empty[Int, Block]
  private val hashTrie = Trie()
  //read by many but write by one
  private val readWriteLock = ReadWriteLock()

  //add Genesis block
  indexToBlock += (Miner.Genesis.index -> Miner.Genesis)
  assert(hashTrie.append(Miner.Genesis.cryptoHash.bytes, Miner.Genesis.index).isRight)

  def append(block: Block): Either[Throwable, Block] = readWriteLock.workByAcquireWriteLock {
    if (block.index > 0 && findByHash(block.parentHash).isEmpty) Left(new RuntimeException(s"Cannot append block: not found parent block with hash ${block.parentHash.toHexString}"))
    else if (indexToBlock.get(block.index).isDefined) Left(new RuntimeException(s"Cannot append block: block with index ${block.index} already exist"))
    else {
      indexToBlock(block.index) = block
      //suppose block hash is unique
      hashTrie.append(block.cryptoHash.bytes, block.index) *> Right(block)
    }
  }

  def findByIndex(index: Int): Option[Block] = readWriteLock.workByAcquireReadLock {
    //time complexity O(1)
    indexToBlock.get(index)
  }

  def findByHash(hash: Hash): Option[Block] = readWriteLock.workByAcquireReadLock {
    //suppose Bytes is Array[Bytes], need to fix.
    val needFind = hash.bytes
    hashTrie.findIndex(needFind).fold(None: Option[Block] /*help type inference*/)(blockIndex => findByIndex(blockIndex))
  }

  // find first common ancestor but not lowest common ancestor
  def common_ancestor(that: Blockchain): Option[Hash] = readWriteLock.workByAcquireReadLock {
    @tailrec
    def findCommonAncestor(blockIndex: Int, foundBlock: Boolean = true, thatBlock: Option[Block] = None): Option[Hash] = {
      if (!foundBlock) None
      else if (thatBlock.nonEmpty) thatBlock.map(_.cryptoHash)
      else {
        val block = findByIndex(blockIndex)
        findCommonAncestor(blockIndex + 1, block.nonEmpty, block.fold(None: Option[Block])(blk => that.findByHash(blk.cryptoHash)))
      }
    }

    findCommonAncestor(1)
  }
}

// Finally, please write some tests to validate some of the properties
// you have encountered in the above description.

// Fun idea just for the discussion: How is this "mini-chain" similar (or not) to Git?

