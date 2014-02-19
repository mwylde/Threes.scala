package com.micahw.threes

import scala.util.Random

sealed abstract class Tile
case object Blank extends Tile
case object OneTile extends Tile
case object TwoTile extends Tile
case class ThreeTile(power : Int) extends Tile

object Tile {
  def toString(t : Tile) = t match {
    case Blank => "_"
    case OneTile => "1"
    case TwoTile => "2"
    case ThreeTile(power) => (3 * (1 << power)).toString
  }

  def toColorString(t : Tile) = (t match {
    case OneTile => Console.BLUE
    case TwoTile => Console.RED
    case _ => Console.RESET
  }) + toString(t)

  def combine(a : Tile, b : Tile) = (a, b) match {
    case (a, Blank) => Some(a)
    case (Blank, b) => Some(b)
    case (OneTile, TwoTile) => Some(ThreeTile(0))
    case (TwoTile, OneTile) => Some(ThreeTile(0))
    case (ThreeTile(p1), ThreeTile(p2)) => {
      if (p1 == p2) {
        Some(ThreeTile(p1 + 1))
      }
      else {
        None
      }
    }
    case _ => None
  }
}

sealed abstract class Direction(val rotations : Int)
case object Left extends Direction(0)
case object Down extends Direction(1)
case object Right extends Direction(2)
case object Up extends Direction(3)

class Board(val board : List[List[Tile]]) {
  override def toString = {
    board.map { row =>
      row.map(Tile.toColorString).mkString("\t")
    }.mkString("\n") + Console.RESET
  }

  private def mod(a : Int, b : Int) = (a % b + b) % b
  private def rotate[T](b : List[List[T]]) = b.transpose.map(_.reverse)
  private def rotateN[T](b : List[List[T]], n : Int) = {
    Range(0, mod(n, 4)).foldLeft(b)((b, _) => rotate(b))
  }

  // Implementing this logic for any particular direction is
  // symmetrical to a single direction. Therefore, for simplicity,
  // we will rotate the board for the requested direction so that we
  // only have to implement one version of this (which will be, for
  // further simplicity, a right-to-left swipe.
  //
  // The algorithm is this:
  //   0. Rotate the board the required number of times.
  //   1. For each row, traverse left-to-right. Start on the second
  //      cell (the first won't be going anywhere).
  //   2. For each cell, check if it can be combined with the previous
  //      cell (possibly because the previous cell is blank)
  //   3. If so, combine the cells and replace the current cell with a
  //      blank. Move on the the next cell.
  //   4. Continue to the next row.
  //   5. Rotate the board back to its correct position.
  def slide(dir : Direction) = {
    val lr = rotateN(board, dir.rotations)
    
    val processed = lr.map{row => {
      // There's probably a nice functional way of doing this, but I
      // can't figure it out right now
      val a = row.toArray
      for (i <- Range(0, row.size - 1)) {
        Tile.combine(a(i), a(i+1)).map{c => {
          a(i) = c
          a(i+1) = Blank
        }}
      }

      a.toList
    }}

    new Board(rotateN(processed, -dir.rotations))
  }

  // def addPiece(dir : Direction) = {
  //   val prob1 = 0.33
  //   val prob2 = 0.33
  //   val prob3 = 0.33
  //   val lr = rotateN(board, dir.rotations)
  //   lr.map(row => row.last
  // }
}

object Board {
  def newInitial = {
    val numOnes = new Random().nextInt(3) + 2
    val numTwos = 6 - numOnes
    val pieces =
      List.make(7, Blank) ++
      List.make(numOnes, OneTile) ++
      List.make(numTwos, TwoTile) ++
      List.make(3, ThreeTile(0))

    new Board(Random.shuffle(pieces).grouped(4).toList)
  }
}

class Game {

}

object Game {
  def main(args : Array[String]) = {
    var b = Board.newInitial
    while (true) {
      println(b)
      val dirString = readLine("> ")
      val dir = dirString match {
        case "u" => Some(Up)
        case "d" => Some(Down)
        case "l" => Some(Left)
        case "r" => Some(Right)
        case "q" => System.exit(0); None
        case _ => None
      }

      dir.foreach(d => b = b.slide(d))
    }
    println(Board.newInitial)
  }
}
