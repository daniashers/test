package applesoranges

/*
  Note: The instructions state:
  "At each step build the simplest possible solution which meets our requirement."
  Therefore considerations such as:
  - Item does not have a price/is unrecognised
  - Undesirable to have hardcoded prices
  - etc
  have NOT been taken into account.
 */

sealed trait ShopItem
case object Apple extends ShopItem
case object Orange extends ShopItem

class Checkout(resolvePrice: ShopItem => BigDecimal = Checkout.DefaultPriceList) {
  def total(items: Seq[ShopItem]): BigDecimal = items.map(resolvePrice(_)).sum
}

object Checkout {
  val DefaultPriceList: ShopItem => BigDecimal = {
    case Apple  => BigDecimal("0.60")
    case Orange => BigDecimal("0.25")
  }
}

object Main extends App {
  val total = new Checkout().total(List(Apple, Apple, Orange, Apple))
  println(total)
}