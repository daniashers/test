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

sealed trait CheckoutEvent
final case class ShopItemAdded(item: ShopItem, price: BigDecimal) extends CheckoutEvent
final case class DiscountApplied(description: String, amount: BigDecimal) extends CheckoutEvent

class Checkout(resolvePrice: ShopItem => BigDecimal = Checkout.DefaultPriceList) {
  def total(items: Seq[ShopItem], applyDiscounts: Boolean = true): BigDecimal = {

    val addedItems = items.map(item => ShopItemAdded(item, resolvePrice(item)))
    val discounts = if (!applyDiscounts) Nil else {
      List.fill(items.count(_ == Apple) / 2)(DiscountApplied("BOGOF on apples", resolvePrice(Apple))) ++
      List.fill(items.count(_ == Orange) / 3)(DiscountApplied("3 for 2 on oranges", resolvePrice(Orange)))
    }

    eventTotal(addedItems ++ discounts)
  }

  private def eventTotal(checkoutItems: Seq[CheckoutEvent]): BigDecimal = checkoutItems.map {
    case ShopItemAdded(_, price) => price
    case DiscountApplied(_, amount) => -amount
  }.sum
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