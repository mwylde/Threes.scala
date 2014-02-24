package com.micahw.threes

import scala.util.Random
import scala.collection.immutable.HashSet

sealed abstract class Tile
case object Blank extends Tile
case object OneTile extends Tile
case object TwoTile extends Tile
case class ThreeTile(power : Int) extends Tile

object Tile {
  def value(t : Tile) = t match {
    case Blank => 0
    case OneTile => 1
    case TwoTile => 2
    case ThreeTile(power) => 3 * (1 << power)
  }

  def toString(t : Tile) = t match {
    case Blank => " "
    case t => Tile.value(t).toString
  }

  def toColorString(t : Tile) =
    backgroundColor(t) + foregroundColor(t) + toString(t) + Console.RESET

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

object BoardUtils { 
  def mod(a : Int, b : Int) = (a % b + b) % b
  def rotate[T](b : List[List[T]]) = b.transpose.map(_.reverse)
  def rotateN[T](b : List[List[T]], n : Int) = {
    Range(0, mod(n, 4)).foldLeft(b)((b, _) => rotate(b))
  }

  val dirs = List(Left, Down, Right, Up)
}
  
class Board(val board : List[List[Tile]],
            val next : Tile,
            val upcoming : Seq[Tile]) {
  // these are the upcoming tiles, from which we will pick randomly

  import BoardUtils._
  
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
    case that: Board => that.toString.equals(toString)
    case _ => false
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
    var changed = HashSet() : Set[Int]
    
    val processed = lr.zipWithIndex.map{case (row, i) => {
      // There's probably a nice functional way of doing this, but I
      // can't figure it out right now
      val a = row.toArray
      for (j <- Range(0, row.size - 1)) {
        Tile.combine(a(j), a(j + 1)).map{c => {
          a(j) = c
          a(j+1) = Blank
          changed = changed + i
        }}
      }

      a.toList
    }}

    (new Board(rotateN(processed, -dir.rotations), next, upcoming), changed)
  }

  def addPiece(dir : Direction, allowedCells : Set[Int]) = {
    // rotate the board so that all swipes are equivalent to a down slide
    val lr = rotateN(board, dir.rotations - 1)
    val choices = lr.head.zipWithIndex.filter({case (c, i) => c == Blank})
    .map(_._2).toSet.intersect(allowedCells).toList

    val row = lr.head.toArray
    row(choices(Random.nextInt(choices.size))) = next
    val newBoard = row.toList :: lr.tail

    // return the rest of the list, or create a new one if we've run out

    // if there's a 48 or higher on the board, 1/21 times choose a bonus card
    // randomly chosen from the card values in (6, m/8) inclusive, where m is
    // the value of the largest card on the board.
    val largestPow = board.flatten.map({
      case ThreeTile(pow) => pow
      case _ => 0
    }).max

    val (newNext, newStack) =
      if (largestPow >= 4 && Random.nextDouble() <= 1 / 21.0) {
        val r = Range(1, largestPow - 2)
        (ThreeTile(r(Random.nextInt(r.size))), upcoming)
      }
      else {
        (upcoming.head,
          if (upcoming.tail.size > 0) {
            upcoming.tail
          }
          else {
            Board.createUpcoming
          })
      }

    new Board(rotateN(newBoard, - (dir.rotations - 1)), newNext, newStack)
  }

  def isAllowed(dir : Direction) = {
    !slide(dir)._1.equals(this)
  }

  def allowedDirs = {
    List(Down, Up, Right, Left).filter(isAllowed)
  }

  def gameStep(dir : Direction) = {
    if (isAllowed(dir)) {
      val (newBoard, changed) = slide(dir)
      Some(newBoard.addPiece(dir, changed))
    }
    else {
      None
    }
  }

  def isDone = {
    allowedDirs.size == 0
  }

  def score = {
    board.flatten.map(Tile.score).sum
  }
}

object Board {
  def newInitial = {
    val (pieces, upcoming) = createUpcoming.splitAt(9)
    val board = Random.shuffle(pieces ++ List.make(7, Blank)).grouped(4).toList

    new Board(board, upcoming.head, upcoming.tail)
  }

  def createUpcoming = {
    val pieces =
      List.make(4, OneTile) ++
      List.make(4, TwoTile) ++
      List.make(4, ThreeTile(0))

    Random.shuffle(pieces).toList
  }
}

object Game {
  def main(args: Array[String]) {
    if (args.size < 1) {
      println("Usage: threes [command] {options}")
      exit(1)
    }
    
    val argList = args.toList
    val command = argList.head

    command match {
      case "human" => human(argList.tail)
      case "ai" => ai(argList.tail)
      case "animate-ai" => animateAI(argList.tail)
      case _ => println("Invalid command (expecting one of " +
                        "[human ai]")
    }
  }

  def ai(args: List[String]) {
    import com.micahw.threes.ai._
    // if (args.size < 1) {
    //   println("you must specify an AI class")
    //   exit(1)
    // }
    
    // val klass = args(0)

    def playGame(ai : AI) = {
      var b = Board.newInitial
      while (!b.isDone) {
        val move = ai.next(b)
        b = b.gameStep(move).getOrElse({
          throw new Exception("Invalid move")
        })
      }
      b
    }

    val n = 50

    val scores = Range(0, n).map(i => {
      val s = playGame(new AI1()).score
      println(s)
      s
    }).sorted
    println("Mean: " + scores.sum / n.asInstanceOf[Double])
    println("Min: " + scores(0))
    println("25%: " + scores(n / 4))
    println("50%: " + scores(n / 2))
    println("75%: " + scores(3 * n / 4))
    println("Max: " + scores(n-1))
  }

  def animateAI(args: List[String]) = {
    import com.micahw.threes.ai._

    var b = Board.newInitial
    val ai = new AI1()
    while (true) {
      println("\033[KNext: " +
              Tile.backgroundColor(b.next) + "  " +
              Console.RESET)
      println(b.output)

      if (b.isDone) {
        println("Final score: " + b.score)
        System.exit(0)
      }
  
      println("\033[20A")    
      b = b.gameStep(ai.next(b)).get
      Thread.sleep(500)
    }
  }
  
  def human(args : List[String]) = {
    val debugMode = args.size > 0 && args(0) == "debug"
    
    var b = Board.newInitial
    var msg = ""
    while (true) {
      println("\033[K" + msg)
      println("\033[KNext: " +
              Tile.backgroundColor(b.next) + "  " +
              Console.RESET)
      println()
      println(b.output)
      println()

      if (b.isDone) {
        println("Game over! Final score: " + b.score)
        System.exit(0)
      }

      val dirString = readLine("\033[K> ")
      val dir = dirString match {
        case "u" => Some(Up)
        case "d" => Some(Down)
        case "l" => Some(Left)
        case "r" => Some(Right)
        case "q" => System.exit(0); None
        case _ => None
      }

      msg = ""
      dir.map(d => b = b.gameStep(d).getOrElse({
        msg = "You can't move that way..."
        b
      })).getOrElse(msg = "Invalid command [r, l, u, d, q]")

      if (!debugMode) {
        println("\033[24A")
      }
    }
  }
}
