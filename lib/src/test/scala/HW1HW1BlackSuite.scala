package lib

import cats.syntax.all.*
import lib.HW1.CalibrationInstrument
import lib.dtos.OptionType
import lib.literals.*
import lib.quantities.*
import lib.quantities.Tenor
import lib.testsyntax.*
import lib.testutils.oneDimPseudoRandomMC

import java.time.LocalDate
import scala.math.*

class HW1HW1BlackSuite extends munit.FunSuite with lib.EitherSyntax:

  val t0 = "2026-10-22"

  case class Case()

  val randCase: Rand[Case] = ???

  test("analytical quanto caplets should satisfy put-call parity"):
    ???

  test("analytical quanto caplets should match MC prices"):
    ???

  test("price should degenerate when payment and libor currencies are the same"):
    ???
