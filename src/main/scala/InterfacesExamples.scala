import java.net.NetworkInterface

import scala.jdk._
import scala.jdk.StreamConverters._

//import collection.convert.ImplicitConversions.`enumeration AsScalaIterator`
import scala.jdk.CollectionConverters.*
object InterfacesExamples extends App {

  val interfaces = NetworkInterface.networkInterfaces().toScala(List)

  val ips   = interfaces.flatMap(_.getInetAddresses().asScala)
  val names = interfaces.map(_.getDisplayName())

  val namesd = interfaces.map(_.getName())

  val mut = interfaces.map(_.getMTU())

  val g = interfaces.flatMap(_.getSubInterfaces().asScala)

  names.zip(mut).foreach(println)

  // ips.foreach(x=>println(x.getHostName()))

  ips.foreach(x => println(x.getHostAddress()))
  // ips.foreach(x=>println(x.getCanonicalHostName()))

  // ips.foreach(x=>println(x.getAddress().head))
  // namesd.foreach(println)

  ips.last.getAddress().foreach(println)
  // g.foreach(x=>println(x.getDisplayName()))

  val interface = NetworkInterface.getByName("lo0")

  val ip = interface
    .inetAddresses()
    .map(_.getAddress().map(_.toString()))
    .map(_.mkString)
    .toScala(List)

  val i = interface.getInetAddresses().asScala.map(_.getAddress().map(_.toString())).map(_.mkString).toList
  ip.foreach(println)
  i.foreach(println)
  println(ip.length)
  println(i.length)
  // NetworkInterface.

}
