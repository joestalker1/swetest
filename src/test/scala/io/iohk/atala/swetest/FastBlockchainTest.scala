package io.iohk.atala.swetest


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Transaction._
import io.iohk.atala.swetest.Base.Bytes

class FastBlockchainTest extends AnyFlatSpec with should.Matchers {
    "A blockchain" should "add mined 1000 blocks and find them by index and its hashes" in {
      val miner = Miner
      val blockNumber = 10000
      val transactions: Seq[Transaction] = Seq("Hello worlds!")
      val blockchain = new FastBlockchain()
      (1 to blockNumber).foldLeft(miner.Genesis) { (prevBlock, i) =>
        val block = miner.mineNextBlock(prevBlock.index + 1, prevBlock.cryptoHash, transactions, prevBlock.miningTargetNumber)
        block.isThisHasBeenMinedProperly() should be(true)
        //add next mined block
        blockchain.append(block) should be(Right(block))
        blockchain.findByIndex(block.index) should be(Option(block))
        blockchain.findByHash(block.cryptoHash) should be(Option(block))
        block
      }
    }

    "A blockchain" should "return Left(Exception()) if block has not been added" in {
      val miner = Miner
      val transactions: Seq[Transaction] = Seq("Hello worlds!")
      val blockchain = new FastBlockchain()
      val block = miner.mineNextBlock(miner.Genesis.index + 1, miner.Genesis.cryptoHash, transactions, miner.Genesis.miningTargetNumber)
      //try to find a block if it's not in the blockchain
      blockchain.findByHash(block.cryptoHash) should be(None)
      blockchain.findByIndex(block.index) should be(None)
      val nextBlock = miner.mineNextBlock(block.index + 1, block.cryptoHash, transactions, block.miningTargetNumber)
      //fail to add a block if its parent has not been added to blockchain
      blockchain.append(nextBlock).isLeft should be(true)
    }


     private def appendBlocks(blockchain: FastBlockchain,firstBlock:Block, blockNumber:Int, transactions: Seq[Transaction]): Unit = {
       (1 to blockNumber).foldLeft(firstBlock){(prevBlock, _) =>
         val block = Miner.mineNextBlock(prevBlock.index + 1, prevBlock.cryptoHash, transactions, prevBlock.miningTargetNumber)
         block.isThisHasBeenMinedProperly() should be(true)
         blockchain.append(block)
         block
       }

     }

     "A blockchain" should "find common ancestor with other blockchain" in {
      val miner = Miner
      val transactions: Seq[Transaction] = Seq("Hello worlds!")
      val blockchain1 = new FastBlockchain()
      val blockchain2 = new FastBlockchain()
      val ancestor = miner.mineNextBlock(miner.Genesis.index + 1, miner.Genesis.cryptoHash, transactions, miner.Genesis.miningTargetNumber)
      blockchain1.append(ancestor).isRight should be(true)
      blockchain2.append(ancestor).isRight should be(true)
      //add blocks to blockchain1
      appendBlocks(blockchain1, ancestor, 3, Seq("blockchain1"))
      //add blocks to blockchain1
      appendBlocks(blockchain2, ancestor, 2, Seq("blockchain2"))
      //find common ancestor
      val commonAncestorHash = blockchain1.common_ancestor(blockchain2)
      val foundCommonAncestor = blockchain1.findByHash(commonAncestorHash.getOrElse(Sha256.Zero_Hash))
      foundCommonAncestor.nonEmpty should be(true)
      ancestor should be(foundCommonAncestor.get)
       //check the next blocks in blockchain1 and blockchain2 are different
      //child of ancestor in blockchain1 is NOT equal to child of ancestor in the blockchain2
      val childOfBlockchain1 = blockchain1.findByIndex(ancestor.index + 1)
      childOfBlockchain1.nonEmpty should be(true)
      val childOfBlockchain2 = blockchain2.findByIndex(ancestor.index + 1)
      childOfBlockchain2.nonEmpty should be(true)
      for{
        child1 <- childOfBlockchain1
        chidl2 <- childOfBlockchain2
      } yield {
        child1 should not be(chidl2)
      }
      val blockchain3 = new FastBlockchain()
      appendBlocks(blockchain3, miner.Genesis, 3, Seq("blockchain3"))
      blockchain1.common_ancestor(blockchain3).isEmpty should be(true)
      blockchain2.common_ancestor(blockchain3).isEmpty should be(true)
    }

}
