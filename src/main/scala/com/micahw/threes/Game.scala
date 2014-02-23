package com.micahw.threes

import scala.util.Random

sealed abstract class Tile
case object Blank extends Tile
case object OneTile extends Tile
case object TwoTile extends Tile
case class ThreeTile(power : Int) extends Tile

object Tile {
  def toString(t : Tile) = t match {
    case Blank => " "
    case OneTile => "1"
    case TwoTile => "2"
    case ThreeTile(power) => (3 * (1 << power)).toString
  }

  def toColorString(t : Tile) = (t match {
    case OneTile => Console.BLUE
    case TwoTile => Console.RED
    case _ => Console.RESET
  }) + toString(t)

  def backgroundColor(t : Tile) = t match {
    case Blank => Console.BLACK_B
    case OneTile => Console.BLUE_B
    case TwoTile => Console.RED_B
    case _ => Console.WHITE_B
  }

  def foregroundColor(t : Tile) = t match {
    case ThreeTile(_) => Console.BLACK + Console.BOLD
    case _ => Console.WHITE + Console.BOLD
  }

  def score(t : Tile) = t match {
    case ThreeTile(pow) => Math.pow(3, pow + 1).asInstanceOf[Int]
    case _ => 0
  }

  def combine(a : Tile, b : Tile) = (a, b) match {
    case (x, Blank) => Some(x)
    case (Blank, y) => Some(y)
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

class Board(val board : List[List[Tile]], val upcoming : Seq[Tile]) {
  // these are the upcoming tiles, from which we will pick randomly

  override def toString = {
    board.map { row =>
      row.map(Tile.toString).mkString(" ")
    }.mkString("\n")
  }

  def toColorString = {
    board.map { row =>
      row.map(Tile.toColorString).mkString(" ")
    }.mkString("\n") + Console.RESET
  }

  /*
+-----+-----+-----+-----+
|  5  | 374 |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
+-----+-----+-----+-----+
   */

  def output = {
    val max = (5 :: board.flatten.map(Tile.toString(_).size)).max
    val cellWidth = max + 2
    val sb = new StringBuilder()
    val rowTop = ((" " + (" " * cellWidth)) * 4) + " \n"

    def center(s : String, w : Int) = {
      val p = w - s.size
      val l = p / 2
      val r = p - l
      " " * l + s + " " * r
    }

    def formatTile(t : Tile) = {
      List(" ", Tile.backgroundColor(t), Tile.foregroundColor(t),
        center(Tile.toString(t), cellWidth),
        Console.RESET).mkString
    }

    board.foreach(row => {
      sb.append(rowTop)
      val vPadding =
        row.map(t => List(" ", Tile.backgroundColor(t), Console.BOLD,
          " " * cellWidth, Console.RESET).mkString)
           .mkString + " \n"

      sb.append(vPadding)
      sb.append(row
        .map(formatTile)
        .mkString + " \n")
      sb.append(vPadding)
    })
    sb.append(rowTop)
    sb.toString()
  }

  override def equals(o: Any) = o match {
    case that: Board => that.toString == toString
    case _ => false
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

    new Board(rotateN(processed, -dir.rotations), upcoming)
  }

  def addPiece(dir : Direction) = {
    val newPiece = upcoming.head
    // rotate the board so that all swipes are equivalent to a down slide
    val lr = rotateN(board, dir.rotations - 1)
    val choices = lr.head.zipWithIndex.filter({case (c, i) => c == Blank})

    val row = lr.head.toArray
    row(choices(Random.nextInt(choices.size))._2) = newPiece
    val newBoard = row.toList :: lr.tail

    // return the rest of the list, or create a new one if we've run out
    val newUpcoming = if (upcoming.tail.size > 0) {
      upcoming.tail
    }
    else {
      Board.createUpcoming
    }

    new Board(rotateN(newBoard, - (dir.rotations - 1)), newUpcoming)
  }

  def gameStep(dir : Direction) = {
    val newBoard = slide(dir)
    if (!newBoard.equals(this)) {
      Some(newBoard.addPiece(dir))
    }
    else {
      None
    }
  }

  def isDone = {
    List(Up, Down, Left, Right)
      .map(slide(_))
      .map(_.toString)
      .forall(_ == toString)
  }

  def score = {
    board.flatten.map(Tile.score).sum
  }
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

    val upcoming = createUpcoming

    new Board(Random.shuffle(pieces).grouped(4).toList, upcoming)
  }

  def createUpcoming = {
    val pieces =
      List.make(4, OneTile) ++
      List.make(4, TwoTile) ++
      List.make(4, ThreeTile(0))

    Random.shuffle(pieces).toList
  }
}

class Game {

}

object Game {
  def main(args : Array[String]) = {
    var b = Board.newInitial
    while (true) {
      println("Next: " + Tile.toColorString(b.upcoming.head) + Console.RESET)
      println()
      println(b.output)
      println()
      val dirString = readLine("> ")
      val dir = dirString match {
        case "u" => Some(Up)
        case "d" => Some(Down)
        case "l" => Some(Left)
        case "r" => Some(Right)
        case "q" => System.exit(0); None
        case _ => None
      }

      dir.foreach(d => b = b.gameStep(d).getOrElse({
        println("You can't move that way...")
        b
      }))

      if (b.isDone) {
        println("Game over! Final score: " + b.score)
        System.exit(0)
      }
    }
  }
}
