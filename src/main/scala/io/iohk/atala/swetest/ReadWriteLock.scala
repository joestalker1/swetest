package io.iohk.atala.swetest

import java.util.concurrent.locks.ReentrantReadWriteLock

case class ReadWriteLock() {
   private val readWriteLock = new ReentrantReadWriteLock()
   private val readLock = readWriteLock.readLock()
   private val writeLock = readWriteLock.writeLock()

  def workByAcquireReadLock[R](work: => R): R = {
    readLock.lock()
    try work finally readLock.unlock()
  }

  def workByAcquireWriteLock[R](work: => R): R = {
     writeLock.lock()
     try work finally writeLock.unlock()
  }
}
