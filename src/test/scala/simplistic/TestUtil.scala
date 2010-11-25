package simplistic

import org.scalatest.BeforeAndAfterEach
import org.scalatest.Suite
import fakesdb.Jetty

object TestUtil {

  val jetty = Jetty.apply(8181)

  jetty.server.start()
  
  val account = new SimpleDBAccount("foo", "bar", "http://localhost:8181")
  
  def flush() {
    account.domain("_flush").create
  }
  
  trait CleanBefore extends BeforeAndAfterEach { self: Suite =>
    override def beforeEach() {
      flush()
    }    
  }
}

