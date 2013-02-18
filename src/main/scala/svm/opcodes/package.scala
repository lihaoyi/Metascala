package svm.parsing

import util.Try

/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 2/15/13
 * Time: 7:34 PM
 * To change this template use File | Settings | File Templates.
 */
package object opcodes {
  object TwoBytes{
    def unapply(s: Seq[Byte]): Option[Int] = Try(s(0) << 8 | s(1)).toOption
  }
}
