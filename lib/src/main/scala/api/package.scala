package lib

package object api:

  def toDayCounter(dc: dtos.DayCounter): lib.DayCounter =
    dc match
      case dtos.DayCounter.Act360 => lib.DayCounter.Act360
      case dtos.DayCounter.Act365 => lib.DayCounter.Act365
