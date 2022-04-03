package io.iohk.atala.swetest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Transaction._

class MinerTest extends AnyFlatSpec with should.Matchers {
  "A miner" should "mine 1000 blocks succesfully" in {
    val miner = Miner
    val blockNumber = 1000
    val transactions: Seq[Transaction] = Seq("Hello worlds!")
    (1 to blockNumber).foldLeft(miner.Genesis) { (prevBlock, i) =>
      val block = miner.mineNextBlock(prevBlock.index + 1, prevBlock.cryptoHash, transactions, prevBlock.miningTargetNumber)
      block.isThisHasBeenMinedProperly should be(true)
      block
    }
  }
}
