package com.micahw.threes

import org.scalacheck.Prop.forAll
import org.scalacheck._

object TileSpecification extends Properties("Tile") {
  val smallInteger = Gen.choose(0, 20)

  property("others") = forAll(smallInteger)(n =>
    Tile.value(ThreeTile(n)) == 3 * Math.pow(2, n).asInstanceOf[Int])
}
