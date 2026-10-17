package lib

import lib.dtos.OptionType

object testsyntax:

  extension [T: DateLike](caplet: lib.Caplet[T])

    def ofKind(kind: OptionType): lib.Caplet[T] =
      new lib.Caplet[T](
        caplet.rate,
        caplet.fixingAt,
        caplet.startAt,
        caplet.endAt,
        caplet.paymentAt,
        caplet.paymentCurrency,
        caplet.strike,
        caplet.discountCurve,
        kind,
        caplet.detachment,
        caplet.fixings
      )

  extension [T: DateLike](swaption: lib.Swaption[T])

    def ofKind(kind: OptionType): lib.Swaption[T] =
      new lib.Swaption[T](
        swaption.rate,
        swaption.fixingAt,
        swaption.strike,
        kind,
        swaption.annuity,
        swaption.discountCurve,
        swaption.detachment,
        swaption.fixings
      )
