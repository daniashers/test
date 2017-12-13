package applesoranges

import org.scalatest.{FlatSpec, FunSuiteLike, Matchers}

class CheckoutTests extends FlatSpec with Matchers {

  val priceList = Checkout.DefaultPriceList
  val checkout = new Checkout(priceList)

  "Checkout" should "handle an empty list" in {
    checkout.total(Nil) shouldBe BigDecimal(0)
  }

  it should "return the correct total without discounts" in {
    checkout.total(List(Apple, Apple, Orange, Apple), applyDiscounts = false) shouldBe BigDecimal("2.05")
  }

  it should "correctly apply BOGOF on apples" in {
    val appleUnitPrice = priceList(Apple)
    checkout.total(List(Apple)) shouldBe appleUnitPrice
    checkout.total(List(Apple, Apple)) shouldBe appleUnitPrice
    checkout.total(List(Apple, Apple, Apple)) shouldBe appleUnitPrice * 2
    checkout.total(List(Apple, Apple, Apple, Apple)) shouldBe appleUnitPrice * 2
  }

  it should "correctly apply 3 for 2 on oranges" in {
    val orangeUnitPrice = priceList(Orange)
    checkout.total(List(Orange)) shouldBe orangeUnitPrice
    checkout.total(List(Orange, Orange)) shouldBe orangeUnitPrice * 2
    checkout.total(List(Orange, Orange, Orange)) shouldBe orangeUnitPrice * 2
    checkout.total(List(Orange, Orange, Orange, Orange)) shouldBe orangeUnitPrice * 3
  }

  it should "correctly handle mixtures of discounts of different types" in {
    val appleUnitPrice = priceList(Apple)
    val orangeUnitPrice = priceList(Orange)
    checkout.total(List(Apple, Apple, Apple, Orange, Orange, Orange, Orange)) shouldBe (appleUnitPrice * 2 + orangeUnitPrice * 3)
  }
}
