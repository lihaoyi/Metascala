package metascala.util

/**
  * A simple doubly-linked list.
  *
  * We implement our own because for some reason, the one in
  * `java.util.LinkedList` and `scala.collection.mutable.DoubleLinkedList`
  * do not allow you to keep references to individual nodes. This is just
  * enough to give us very-efficient insertions and removals as long as we keep
  * a reference to the `Link` after insertion.
  */
class LinkedList[T]{
  var head: Link[T] = null
  def prepend(v: T): Link[T] = {
    val link = new Link(v)
    if (head != null) {
      head.prev = link
      link.next = head
    }

    head = link
    link
  }
  def remove(v: Link[T]) = {
    if (v == head) head = v.next
    if (v.prev != null){
      v.prev.next = v.next
    }
    if (v.next != null){
      v.next.prev = v.prev
    }
    v.next = null
    v.prev = null
  }
  def foreach(f: T => Unit): Unit = {
    var current = head
    while(current != null){
      f(current.value)
      current = current.next
    }
  }
}

class Link[T](val value: T){
  var prev: Link[T] = null
  var next: Link[T] = null
}