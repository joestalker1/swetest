package io.iohk.atala.swetest

import java.security.MessageDigest
import io.iohk.atala.swetest.Base._

import scala.annotation.tailrec


// Some base definitions
object Base {
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
  val toNumber: Number = Number(1, bytes)

  val toHexString: String = Base.toHexString(bytes)

  override def equals(other: Any): Boolean = other match {
    case p: Hash => bytes.sameElements(p.bytes)
    case _ => false
  }
}


object Hash {
  def fromBytes(xs: Byte*): Hash = Hash(xs.toArray)
}

// Hashes are produced by a cryptographic function and in "mini-chain" we
// use SHA-256, which always generates a 32-byte (256-bit) value.
object Sha256 {
  val NumberOfBytes = 32
  //use theadlocal because TheDigest is not threadsafe and may be bottleneck so naive approach to
  // has digest for every thread from pool
  val TheDigest = new ThreadLocal[MessageDigest] {
    override def initialValue = MessageDigest.getInstance("SHA-256")
  }


  // We pre-compute the hash of an empty array of 32 bytes.
  // We call this the "Zero_Hash".
  val Zero_Hash: Hash = Hash(Bytes(NumberOfBytes))

  //TheDigest is not thread-safe and it may be used in multithreaded env.
  def apply(bytes: Bytes*): Hash = synchronized {
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
  def verifyThisHasBeenMinedProperly: Unit =
    assert(isThisHasBeenMinedProperly)

  val isThisHasBeenMinedProperly: Boolean =
    cryptoHash.toNumber < miningTargetNumber
}

object Miner {
  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  final val Genesis = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Sha256.Zero_Hash, // Let's assume this is by definition for the Genesis block.
    transactions = Seq(Transaction("Hello Blockchain, this is Genesis :)")),
    StdMiningTargetNumber
  )

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
    @tailrec
    def findNonce(curNonce: Long): Long = {
      val block = Block(index, parentHash, transactions, miningTargetNumber, curNonce)
      // println(s"Applied nonce $curNonce for a block ${block.cryptoHash.toHexString}: mined properly is ${block.isThisHasBeenMinedProperly}")
      if (block.isThisHasBeenMinedProperly) curNonce
      else findNonce(curNonce + 1)
    }

    //find nonce for O(N)
    val nonce = findNonce(Long.MinValue)
    Block(index, parentHash, transactions, miningTargetNumber, nonce)
  }
}

// A Blockchain is a sequence of blocks, each one having an index.
// The index of a block is the index of its parent plus one.
// A Blockchain always has a genesis block at index 0, which is the lowest index.
sealed trait Blockchain {
  // Add a block to the chain.
  // The return type is up to you, as explained in the definition of Unknown.
  def append(block: Block): Either[Throwable, Blockchain]

  // Find a block by index.
  def findByIndex(index: Int): Option[Block]

  // Find a block by hash.
  def findByHash(hash: Hash): Option[Block]

  // Find a common ancestor between this blockchain and that blockchain.
  def common_ancestor(that: Blockchain): Either[Throwable, Option[Hash]]
}

// Implement an in-memory blockchain that internally has an indexing data structure.
// The purpose of this internal data structure is to avoid traversing the linked list
// of blocks when answering queries like findByIndex.

case class FastBlockchain(private val indexToBlock: Seq[Block] = Seq(Miner.Genesis),
                          private val hashToBlock: Map[String, Block] = Map(Miner.Genesis.cryptoHash.toHexString -> Miner.Genesis)) extends Blockchain {

  private def error(msg: String) = {
    Left(new RuntimeException(msg))
  }

  //add Genesis block
  override def append(block: Block): Either[Throwable, Blockchain] = {
    if (!block.isThisHasBeenMinedProperly) error(s"Cannot add this block: this block is mined propery")
    else if (block.index > 0 && findByHash(block.parentHash).isEmpty) error(s"Cannot append block: not found parent block with hash ${block.parentHash.toHexString}")
    else if (indexToBlock.size != block.index) error(s"Cannot append block: expect block with index ${indexToBlock.size}")
    else
      Right(FastBlockchain(indexToBlock :+ block, hashToBlock + (block.cryptoHash.toHexString -> block)))
  }

  override def findByIndex(index: Int): Option[Block] = {
    if (index >= 0 && index < indexToBlock.size) Option(indexToBlock(index))
    else None
  }

  override def findByHash(hash: Hash): Option[Block] = {
    Option(hash).map(h => hashToBlock.get(h.toHexString)).getOrElse(None)
  }

  // find lowest common ancestor for O(lg(N))
  override def common_ancestor(that: Blockchain): Either[Throwable, Option[Hash]] = {
    if (that == null) {
      error("Argument is null")
    } else {
      @tailrec
      def findLCA(lo: Int, hi: Int): Option[Hash] = {
        if (lo >= hi && lo == 1) None
        else if (lo >= hi) findByIndex(lo - 1).flatMap(hash => that.findByHash(hash.cryptoHash).map(_.cryptoHash))
        else {
          val mid = lo + (hi - lo) / 2
          val ifBlock = findByIndex(mid)
          val ifThatBlock = ifBlock.fold(None: Option[Block] /*ifBlock always exists*/)(blk => that.findByHash(blk.cryptoHash))
          if (ifThatBlock.isEmpty) findLCA(lo, mid)
          else findLCA(mid + 1, hi)
        }
      }
      //skip genesis block and start searching from 1
      Right(findLCA(1, indexToBlock.size - 1))
    }
  }
}


