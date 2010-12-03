package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AccountSuite extends WordSpec with ShouldMatchers with TestUtil.CleanBefore with TestUtil.StopAndStartServer {
  import TestUtil._

  "Account.domains" should {

    "return zero when SimpleDB is empty" in {
      account.domains should have size (0)
    }

    "return one domain after one has been created" in {
      account.domain("test").create()
      account.domains.map(_.name).toSet should be === Set("test")
    }

    "support hundreds of domains" in {
      val domains = (1 to 1000) map ("domain-"+_)
      for (d <- domains) {
        account.domain(d).create()
      }

      account.domains.map(_.name).toSet should be === domains.toSet
    }
  }
}
