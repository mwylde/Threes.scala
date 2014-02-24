package com.micahw.threes.ai

import com.micahw.threes._

class AI1 extends AI {
  import BoardUtils._
  val lookAhead = 1

  def next(board : Board) = {
    val allowed = board.allowedDirs
    dirs.map(d => evaluate(board, d, lookAhead))
      .maxBy(_._2.map(_.score).getOrElse(0))._1
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
      }).maxBy(_._2.map(_.score).getOrElse(0))
    }
  }
}
