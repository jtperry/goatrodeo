package goatrodeo.util

class WorkQueue[T](
    private var data: Vector[T] = Vector(),
    private var done: Boolean = false
) {
  def addItem(item: T): Unit = {
    this.synchronized {
      this.data :+= item
      this.notifyAll()
    }
  }

  def addItems(items: Seq[T]): Unit = {
    this.synchronized {
      this.data = this.data ++ items
      this.notifyAll()
    }
  }

  def close(): Unit = {
    this.synchronized {
      done = true
      this.notifyAll()
    }
  }

  def closed(): Boolean = {
    this.synchronized {
      this.done && this.data.isEmpty
    }
  }

  def getBatch(): Vector[T] = {
    this.synchronized {
      while (!this.done && this.data.isEmpty) {
        this.wait()
      }
      val ret = this.data
      this.data = Vector.empty
      ret
    }
  }

  def into[O](f: T => O): WorkQueue[O] = {
    val other = this.synchronized {
      this.data.map(f)
    }
    WorkQueue(other, true)
  }

}

object WorkQueue {
  def from[T](in: Vector[T]): WorkQueue[T] = {
    val ret = WorkQueue[T]()
    ret.addItems(in)
    ret.close()
    ret
  }
}
