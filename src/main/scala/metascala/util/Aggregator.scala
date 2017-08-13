package metascala.util

import scala.reflect.ClassTag

object Agg {

  def apply[T: ClassTag](xs: T*) = {
    val x = new Aggregator[T]()
    xs.foreach(x.append)
    x
  }

  def from[T: ClassTag](xs: TraversableOnce[T]) = {
    val x = new Aggregator[T](1)
    xs.foreach(x.append)
    x
  }

  def fill[T: ClassTag](i: Int)(f: => T) = {
    val x = new Aggregator[T](i)
    for (_ <- 0 until i) x.append(f)
    x
  }

  val empty0 = new Aggregator[Nothing]
  def empty[T: ClassTag]: Agg[T] = empty0

  def unapplySeq[T](x: Array[T]): Option[IndexedSeq[T]] = Some(x.toVector)
}

/**
  * An immutable view over an unboxed array of `T`s. Basically the most
  * efficient way you can store large arrays of primitive types while enforcing
  * both immutability and unboxedness (`scala.Array` is mutable, `IndexedSeq`
  * may or may not be unboxed)
  */
sealed trait Agg[@specialized(Int, Long) +T] extends (Int => T) with TraversableOnce[T]{

  def apply(i: Int): T
  def length: Int
  def toArray[B >: T: ClassTag]: Array[B]

  def compact(): Agg[T]

  def zipWithIndex: Agg[(T, Int)]
  def withFilter(f: T => Boolean): Agg[T]
  def filter(f: T => Boolean): Agg[T]
  def map[V: ClassTag](f: T => V): Agg[V]
  def collect[V: ClassTag](pf: PartialFunction[T, V]): Agg[V]
  def flatMap[V: ClassTag](f: T => TraversableOnce[V]): Agg[V]

  def last: T
  def lastOption: Option[T]
  def head: T
  def contains[B >: T](t: B): Boolean

  def headOption: Option[T]
  def indices: Range
  def indexOf[B >: T](t: B): Int
  def groupBy[V](key: T => V): Map[V, Agg[T]]
  def reverseIterator: Iterator[T]
  def iterator: Iterator[T]
  def indexWhere(f: T => Boolean): Int
  def take(n: Int): Agg[T]
  def drop(n: Int): Agg[T]
  def slice(start: Int, end: Int): Agg[T]
  def partition(f: T => Boolean): (Agg[T], Agg[T])
  def ++[B >: T: ClassTag](other: TraversableOnce[B]): Agg[B]
}


/**
  * A custom growable-array of unboxed `T`s. Effectively a more memory-efficient
  * mutable.ArrayBuffer when used with primitive `T`s
  */
class Aggregator[@specialized(Int, Long) T: ClassTag](initialSize: Int = 1) extends Agg[T] {
  // Can't be `private` because it makes `@specialized` behave badly
  protected[this] var data = new Array[T](initialSize)
  protected[this] var length0 = 0

  def length = length0
  def apply(i: Int) = data(i)
  def append(i: T) = {
    if (length >= data.length) {
      // Grow by 3/2 + 1 each time, same as java.util.ArrayList. Saves a bit
      // of memory over doubling each time, at the cost of more frequent
      // doublings,
      val newData = new Array[T](data.length * 3 / 2 + 1)
      System.arraycopy(data, 0, newData, 0, length)
      data = newData
    }
    data(length) = i
    length0 += 1
  }
  def appendAll(others: TraversableOnce[T]) = {
    for(i <- others) append(i)
  }
  def zipWithIndex = {
    val x = new Aggregator[(T, Int)](length)
    for(i <- 0 until length) x.append(this(i) -> i)
    x
  }
  def withFilter(f: T => Boolean): Agg[T] =  filter(f)
  def filter(f: T => Boolean): Agg[T] = {
    val x = new Aggregator[T](length)
    for(i <- 0 until length) if (f(this(i))) x.append(this(i))
    x
  }

  def map[V: ClassTag](f: T => V): Agg[V] = {
    val x = new Aggregator[V](length)
    for(i <- 0 until length) x.append(f(this(i)))
    x
  }
  def collect[V: ClassTag](pf: PartialFunction[T, V]): Agg[V] = {
    val b = new Aggregator[V]()
    foreach(pf.runWith(b.append(_)))
    b
  }
  def flatMap[V: ClassTag](f: T => TraversableOnce[V]): Agg[V] = {
    val x = new Aggregator[V]()
    for(i <- 0 until length) {
      for(v <- f(this(i))){
        x.append(v)
      }
    }
    x
  }
  def indexOf[B >: T](i: B) = {
    data.indexOf(i) match{
      case x if x < length0 => x
      case _ => -1
    }
  }
  def last = data(length0 - 1)

  def lastOption = data.lift(length0 - 1)
  def head = data(0)
  def headOption = data.lift(0)
  def indices = 0 until length
  def compact() = {
    data = data.take(length)
    this
  }
  // Members declared in scala.collection.GenTraversableOnce
  def isTraversableAgain = true
  def toIterator = data.iterator.take(length)
  def toStream = toIterator.toStream
  // Members declared in scala.collection.TraversableOnce
  def copyToArray[B >: T](xs: Array[B],start: Int,len: Int) = {
    System.arraycopy(data, 0, xs, start, len)
  }
  def exists(p: T => Boolean) = indexWhere(p) match{
    case -1 => false
    case _ => true
  }
  def find(p: T => Boolean): Option[T] = {
    var i = 0
    var out: Option[T] = None
    while(i < length && (out eq None)){
      if (p(this(i))) out = Some(this(i))
      i += 1
    }
    out
  }
  def forall(p: T => Boolean) = {
    var result = true
    foreach[Unit](x => result = result & p(x))
    result
  }
  def foreach[U](f: T => U) = {
    for(i <- 0 until length){
      f(this(i))
    }
  }
  def hasDefiniteSize = true
  def isEmpty = length == 0
  def seq: scala.collection.TraversableOnce[T] = this
  def toTraversable = data.take(length)
  override def toArray[B >: T: ClassTag] = {
    val x = new Array[B](length0)
    var i = 0
    while(i < length0){
      x(i) = data(i)
      i += 1
    }
    x
  }
  def groupBy[V](f: T => V) = {
    this.toArray.groupBy(f).map{case (k, v) => (k, Agg.from(v))}
  }
  def contains[B >: T](t: B) = indexOf(t) != -1
  override def hashCode(): Int = {
    var total = 0
    for(i <- 0 until length) total += data(i).hashCode()
    total
  }
  override def equals(o: Any): Boolean = o match{
    case t: Agg[T] if t.length == this.length =>
      var equalSoFar = true
      for(i <- 0 until length) equalSoFar = equalSoFar & (this(i) == t(i))
      equalSoFar
    case _ => false
  }
  def reverseIterator: Iterator[T] = new Iterator[T]{
    var currentIndex = Aggregator.this.length
    def hasNext = {
      currentIndex > 0
    }

    def next() = {
      currentIndex -= 1
      data(currentIndex)
    }
  }
  def iterator: Iterator[T] = new Iterator[T]{
    var currentIndex = 0
    def hasNext = currentIndex < Aggregator.this.length

    def next() = {
      val res = data(currentIndex)
      currentIndex += 1
      res
    }
  }
  def indexWhere(f: T => Boolean) = {
    var result = -1
    var i = 0
    while(i < length && result == -1){
      if (f(data(i))) result = i
      i += 1
    }
    result
  }

  def take(n: Int) = {
    val realN = math.min(n, length)
    val res = new Aggregator[T](realN)
    for(i <- 0 until realN){
      res.append(data(i))
    }
    res
  }
  def drop(n: Int) = {
    val res = new Aggregator[T](length - n)
    for(i <- n until length){
      res.append(data(i))
    }
    res
  }
  def slice(start: Int, end: Int) = {
    val realEnd = math.min(end, length)
    val realStart = math.max(start, 0)
    val res = new Aggregator[T](math.max(0, realEnd - realStart))
    for(i <- realStart until realEnd){
      res.append(data(i))
    }
    res
  }

  def partition(f: T => Boolean) = {
    val truthy = new Aggregator[T]()
    val falsy = new Aggregator[T]()
    for(i <- this){
      if (f(i))truthy.append(i)
      else falsy.append(i)
    }
    (truthy, falsy)
  }
  def ++[B >: T: ClassTag](other: TraversableOnce[B]) = {
    val res = new Aggregator[B]()
    res.appendAll(this)
    res.appendAll(other)
    res
  }
  override def toString = s"Agg(${data.iterator.take(length).mkString(", ")})"
}

