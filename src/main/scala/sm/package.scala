/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 3/12/13
 * Time: 10:00 PM
 * To change this template use File | Settings | File Templates.
 */
package object sm {
  private[sm] implicit class castable(val x: Any) extends AnyVal{
    def cast[T] = x.asInstanceOf[T]
  }
}
