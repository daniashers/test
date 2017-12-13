package applesoranges

import org.scalatest.{FlatSpec, FunSuiteLike, Matchers}

class CheckoutTests extends FlatSpec with Matchers {
  "Checkout" should "handle an empty list" in {
    new Checkout().total(Nil) shouldBe BigDecimal(0)
  }

  it should "return the correct total" in {
    new Checkout().total(List(Apple, Apple, Orange, Apple)) shouldBe BigDecimal("2.05")
  }
}
