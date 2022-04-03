package io.iohk.atala.swetest


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Transaction._

class FastBlockchainTest extends AnyFlatSpec with should.Matchers {
  "A blockchain" should "add mined 100 blocks and find them by index and its hashes" in {
    val miner = Miner
    val blockNumber = 100
    val transactions: Seq[Transaction] = Seq("Hello worlds!")
    val blockchain:Blockchain = FastBlockchain()
    (1 to blockNumber).foldLeft((blockchain, miner.Genesis)) { (prevBlockBlk,_) =>
      val block = miner.mineNextBlock(prevBlockBlk._2.index + 1, prevBlockBlk._2.cryptoHash, transactions, prevBlockBlk._2.miningTargetNumber)
      block.isThisHasBeenMinedProperly should be(true)
      //add next mined block
      val updatedBlockchain = prevBlockBlk._1.append(block)
      updatedBlockchain.isRight should be(true)
      updatedBlockchain.foreach { blk =>
        blk.findByIndex(block.index) should be(Option(block))
        blk.findByHash(block.cryptoHash) should be(Option(block))
      }
      (updatedBlockchain.fold(ex=> throw ex, identity[Blockchain]),block)
    }
  }

  "A blockchain" should "return Left(Exception()) if block has not been added" in {
    val miner = Miner
    val transactions: Seq[Transaction] = Seq("Hello worlds!")
    val blockchain = FastBlockchain()
    val block = miner.mineNextBlock(miner.Genesis.index + 1, miner.Genesis.cryptoHash, transactions, miner.Genesis.miningTargetNumber)
    //try to find a block if it's not in the blockchain
    blockchain.findByHash(block.cryptoHash) should be(None)
    blockchain.findByIndex(block.index) should be(None)
    val nextBlock = miner.mineNextBlock(block.index + 1, block.cryptoHash, transactions, block.miningTargetNumber)
    //fail to add a block if its parent has not been added to blockchain
    blockchain.append(nextBlock).isLeft should be(true)
  }


  private def appendBlocks(blockchain: Blockchain, firstBlock: Block, blockNumber: Int, transactions: Seq[Transaction]): Blockchain = {
    (1 to blockNumber).foldLeft((firstBlock, blockchain)) { (prevBlockBlk, _) =>
      val block = Miner.mineNextBlock(prevBlockBlk._1.index + 1, prevBlockBlk._1.cryptoHash, transactions, prevBlockBlk._1.miningTargetNumber)
      block.isThisHasBeenMinedProperly should be(true)
      val updatedBlockchain = prevBlockBlk._2.append(block)
      updatedBlockchain.isRight should be(true)
      val newBlk: Blockchain = updatedBlockchain.fold(ex => throw ex, identity[Blockchain])
      (block, newBlk)
    }._2
  }

  "A blockchain" should "find common ancestor with other blockchain" in {
    val miner = Miner
    val transactions: Seq[Transaction] = Seq("Hello worlds!")
    var blockchain: Blockchain = FastBlockchain()
    var thatBlockchain: Blockchain = FastBlockchain()
    val commonBlock = miner.mineNextBlock(miner.Genesis.index + 1, miner.Genesis.cryptoHash, transactions, miner.Genesis.miningTargetNumber)
    val appended1 = blockchain.append(commonBlock)
    appended1.isRight should be(true)
    appended1.foreach(blk => blockchain = blk)
    val appended2 = thatBlockchain.append(commonBlock)
    appended2.isRight should be(true)
    appended2.foreach(blk => thatBlockchain = blk)
    //add new blocks to blockchain
    blockchain = appendBlocks(blockchain, commonBlock, 30, Seq("blockchain"))
    //add new blocks to thatBlockchain
    thatBlockchain = appendBlocks(thatBlockchain, commonBlock, 40, Seq("thatBlockchain"))
    //find common ancestor
    val commonAncestorHash = blockchain.common_ancestor(thatBlockchain)
    commonAncestorHash.isRight should be(true)
    commonAncestorHash.foreach{hash =>
        hash.nonEmpty should be(true)
        hash.exists(h => h == commonBlock.cryptoHash) should be(true)
    }

    var blockchain2:Blockchain = FastBlockchain()
    blockchain2 = appendBlocks(blockchain2, miner.Genesis, 10, Seq("blockchain2"))
    val foundHash = blockchain.common_ancestor(blockchain2)
    foundHash.isRight should be(true)
    foundHash.foreach { hash =>
         hash.isEmpty should be(true)
    }
  }

}
