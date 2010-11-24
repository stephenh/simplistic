// Cut & paste into Scala REPL
import org.sublime.amazon.simpleDB.api._

val account = new SimpleDBAccount(System.getenv("AWS_ACCESS_KEY_ID"), System.getenv("AWS_SECRET_ACCESS_KEY"))

// List all domains
account.domains.toList

// List all items in mydomain
account.domain("mydomain").items.toList

// Create a new item with a single attribute "bar" and value "baz"
account.domain("mydomain") item ("foo") += ("bar" -> "baz")

// Query mydomain and print results
account.select ("select * from mydomain") foreach { e => println(e.name) }

