package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class QuerySuite extends WordSpec with ShouldMatchers with TestUtil.CleanBefore with TestUtil.StopAndStartServer {
  import TestUtil._

  val testDomain = account.domain("test")

  override def beforeEach() {
    super.beforeEach()
    testDomain.create()
  }
  
  "Domain.findFirst" should {
    import Attributes._
    import Query._

    val attr = attribute("test")
    def addItem(s: String) { testDomain.unique += attr(s) }
    def findFirst(s: String) = testDomain findFirst (attr is s)
    
    "return Some(firstItem) when such item exists" in {
      addItem("foo")
      findFirst("foo").isDefined should be === true
    }

    "return None when no such item exists" in {
      addItem("foo")
      findFirst("bar").isDefined should be === false
    }

    "return Some(firstItem) when multiple such items exists" in {
      addItem("foo")
      addItem("foo")
      findFirst("foo").isDefined should be === true
    }
  }

}
