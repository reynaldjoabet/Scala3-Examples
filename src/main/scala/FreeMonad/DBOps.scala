package FreeMonad


trait DBOps[A]
case class Create[A](key: String, value: A) extends DBOps[Unit]
case class Read[A](key: String) extends DBOps[A]
case class Update[A](key: String, value: A) extends DBOps[A]
case class Delete(key: String) extends DBOps[Unit]
object DBOps {
 
  implicit val dbOps2IO: DBOps ~> IO = new (DBOps ~> IO) {
    override def apply[A](fa: DBOps[A]): IO[A] = ???
  }
}
