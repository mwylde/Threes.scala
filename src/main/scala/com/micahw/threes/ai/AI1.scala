package com.micahw.threes.ai

import com.micahw.threes._

class AI1 extends AI {
  import BoardUtils._
  val lookAhead = 1

  def next(board : Board) = {
    val allowed = board.allowedDirs
    allowed.map(d => evaluate(board, d, lookAhead))
    .maxBy(_._2.map(score(_, lookAhead))
           .getOrElse(Double.NegativeInfinity))._1
  }

  case class Param(name : String, f : (Board => Double), w : Double)

  val params = List(
    Param("FreeSquares",
          (b) => b.board.flatten.filter(_ == Blank).size / 16.0, 1000.0),
    Param("GameOver",
          (b) => if (b.isDone) { 1.0 } else { 0.0 }, -20.0),
    Param("Score",
          (b) => b.score / Math.pow(3, 7), 100.0),
    Param("NumOnesTwos",
          (b) => b.board.flatten.filter(
            t => t == OneTile || t == TwoTile).size / 16.0, -2.0)
    )

  def score(b : Board, iters : Int) : Double = {
    // val s = b.score
    // val factor = s / 10.0
    // val free = b.board.flatten.filter(_ == Blank).size
    // (s + free * factor - (if (b.isDone) { factor * 1.5 } else  { 0 })) *
    //   (1/Math.pow(2.0, lookAhead - iters ))

    params.map(p => p.f(b) * p.w).sum * (1/Math.pow(2.0, lookAhead - iters ))
  }

  def evaluate(board : Board, d : Direction, iters : Int)
  : (Direction, Option[Board]) = {
    val bp = board.gameStep(d)
    if (iters == 0 || bp.isEmpty || bp.get.isDone) {
      (d, bp)
    }
    else {
      dirs.map(d => {
        evaluate(board, d, iters - 1)
      }).maxBy(_._2.map(score(_, iters - 1))
               .getOrElse(Double.NegativeInfinity))
    }
  }
}
