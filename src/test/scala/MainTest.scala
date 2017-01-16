import org.scalatest.{FlatSpec, Matchers}

class MainTest extends FlatSpec with Matchers {

  it should "pop values in last-in-first-out order" in {
    Main.plusOne(2) should be (3)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    a [Exception] should be thrownBy {
      Main.plusOne(5)
    }
  }

}
