package FreeMonad

import scala.concurrent.Future

sealed trait FileOp[A]
case class ReadFile(path: String) extends FileOp[String]
case class WriteFile(path: String, content: String) extends FileOp[Unit]

type FileProgram[A] = Free[FileOp, A]

def readFile(path: String): FileProgram[String] = Free.liftF(ReadFile(path))
def writeFile(path: String, content: String): FileProgram[Unit] =
  Free.liftF(WriteFile(path, content))

//val localInterpreter: FileOp ~> Id = ???
val cloudInterpreter: FileOp ~> Future = new {
  override def apply[A](fa: FileOp[A]): Future[A] = fa match {
    case ReadFile(path)           => Future.successful(???)
    case WriteFile(path, content) => Future.successful(???)
  }
}

val program: FileProgram[String] = for {
  content <- readFile("local/path/to/file.txt")
  _ <- writeFile("cloud/path/to/file.txt", content)
} yield content

val result: Future[String] = program.foldMap(cloudInterpreter)
