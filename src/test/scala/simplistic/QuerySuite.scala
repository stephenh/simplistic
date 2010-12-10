package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class QuerySuite extends WordSpec with ShouldMatchers with TestUtil.CleanBefore with TestUtil.StopAndStartServer {
  import TestUtil._

  val testDomain = account.domain("larry_test")

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
  
  
  "Domain.query" should {
    import Query._
    import TestDomainData._

    "simple query expression" in {
      setupData(testDomain)
      (testDomain (visits > 16) map (user(_))).toSet should be === Set("jon", "jack")
    }
    
    "range query expression" in {
      setupData(testDomain)
      (testDomain (visits > 16 and visits < 50) map (user(_))).head should be === "jon"
    }
    
    "range query with sorting" in {
      setupData(testDomain)
      (testDomain (visits > 1 and visits < 50 sort visits desc) map (user(_))).toList should be === List("jon", "alice", "robin")
    }
  }
  
  "Domain.items" should {
    import Attributes._
    
    val attr = attribute("test")
    def addItem(s: String) { testDomain.unique += attr(s) }
    
    "return all items" in {
      addItem("a")
      addItem("b")
      addItem("c")
      
      (testDomain.items map {(item) => attr(item.attributes)}).toSet should be === Set("a", "b", "c")
    }
  }
}

object TestDomainData {
  import Attributes._
  import Conversions._
  
  val user = attribute("user")
  val startDate = attribute("startDate", ISO8601Date)
  val visits = attribute("visits", PositiveInt)
  val tags = attribute("tags")
    
  def setupData(d: Domain) = {
    d.unique += (user("robin"), startDate(new java.util.Date()), visits(3))
    d.unique += (user("jon"), startDate(new java.util.Date()), visits(20))
    d.unique += (user("alice"), startDate(new java.util.Date()), visits(15))
    d.unique += (user("jack"), startDate(new java.util.Date()), visits(100))
  }
}
