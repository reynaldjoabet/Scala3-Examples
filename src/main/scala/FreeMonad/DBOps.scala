package FreeMonad

sealed trait DBOps[A]
case class Create[A](key: String, value: A) extends DBOps[Unit]
case class Read[A](key: String)             extends DBOps[A]
case class Update[A](key: String, value: A) extends DBOps[A]
case class Delete(key: String)              extends DBOps[Unit]

object DBOps {

  implicit val dbOps2IO: DBOps ~> IO = new (DBOps ~> IO) {

    override def apply[A](fa: DBOps[A]): IO[A] = fa match {
      case Create(key, value) => IO(() => ???)
      case Read(key)          => IO(() => ???)
      case Update(key, value) => IO(() => ???)
      case Delete(key)        => IO(() => ???)
    }

  }

  implicit val dbOps2IO2: ~>[DBOps, IO] = new ~>[DBOps, IO] {

    override def apply[A](fa: DBOps[A]): IO[A] = fa match {
      case Create(key, value) => IO(() => ???)
      case Read(key)          => IO(() => ???)
      case Update(key, value) => IO(() => ???)
      case Delete(key)        => IO(() => ???)
    }

  }

}
// For our little program, we can interpret it with foldMap, if we can create a natural transformation from our type DBOps to some other type that will actually perform the actions described by the program
