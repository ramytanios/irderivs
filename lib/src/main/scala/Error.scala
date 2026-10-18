package lib

import lib.dtos.Currency

abstract class Error(msg: String) extends RuntimeException(msg)

object Error:

  class Generic(msg: String) extends Error(msg)

  def apply(msg: String): Error = Error.Generic(msg)

  class QuantoNotSupported(paymentCcy: Currency, rateCcy: Currency)
      extends Error(s"payment currency: $paymentCcy, rate currency: $rateCcy")

  class PaymentConvexityNotSupported[T](tEnd: T, tPay: T)
      extends Error(s"end date: $tEnd, payment date: $tPay")
