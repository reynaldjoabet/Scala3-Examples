import java.net.InetAddress

object Anotherexample extends App {

  val host = InetAddress.getLocalHost();

  val l = InetAddress.getLoopbackAddress()

  val h = InetAddress.getByName("localhost")

  val host2 = InetAddress.getByAddress(Array(0, 0, 0, 0))

  val hosts2 = InetAddress.getAllByName("0.0.0.0")

  // println(hosts.getHostAddress())

  // println(hosts.getHostName())
  // println(hosts.getCanonicalHostName())
  // println(host.getHostAddress())

  println(host.getHostName())
  println(host.getCanonicalHostName())

  println(host2.getHostAddress())

  println(h.getHostAddress())
  println(h.getHostName())
  println(h.getCanonicalHostName())
  println(host2.getHostName())
  println(host2.getCanonicalHostName())

  hosts2.foreach(x => println(x.getCanonicalHostName()))
  val d: List[Int] = List(3)
  println(hosts2.length)
}
