package lib

import lib.dtos.Currency

case class Measure[T](tStar: T, currency: Currency)
