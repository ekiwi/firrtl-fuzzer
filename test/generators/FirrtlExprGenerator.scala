package generators

// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

import firrtl.PrimOps._
import firrtl.ir._

import scala.math._


/*
 * Known shortcomings:
 * - does not generate clock expressions
 * - no good way to limit expression size
 *
 * Possible Improvements:
 * - propagate expected type down the stack
 *


 */

trait FirrtlGenerator {
  private val Alpha : Seq[String] = (('a' to 'z') ++ ('A' to 'Z') ++ Seq('_')).map(_.toString)
  private val AlphaNum : Seq[String] = Alpha ++ ('0' to '9').map(_.toString)

  protected def getWidth(tpe: Type) : Int = tpe match {
    case UIntType(IntWidth(w)) => w.toInt
    case SIntType(IntWidth(w)) => w.toInt
    case other => throw new RuntimeException(s"Cannot get width for type: $other")
  }

  protected def isSigned(tpe: Type): Boolean = tpe match {
    case SIntType(_) => true
    case _ => false
  }

  protected def identifier(rand: Random, maxLength: Int = 20): String = {
    // (12 Details about Syntax):
    // > The following characters are allowed in identifiers: upper and lower case letters, digits, and _.
    // > Identifiers cannot begin with a digit.
    assert(maxLength >= 1)
    val len = rand.nextInt(1, maxLength)
    val start = rand.choose(Alpha)
    if(len == 1) { start } else {
      start + (1 until len).map(_ => rand.choose(AlphaNum)).reduce(_ + _)
    }
  }
}

object FirrtlPrimOpGenerator {
  val UnOps = Seq(Pad, AsUInt, AsSInt, Shl, Shr, Cvt, Neg, Not, Andr, Orr, Xorr, Bits, Head, Tail)
  val BinOps = Seq(Add, Sub, Mul, Div, Rem, Lt, Leq, Gt, Geq, Eq, Neq, And, Or, Xor, Cat)
  val DynShiftOps = Seq(Dshl, Dshr)
}

case class FirrtlPrimOpSettings(
  MaxShiftLeft: Int = 33,
  MaxPad: Int = 33,
  MaxDynamicShiftLeftBits: Int = 4,
  AvailablePrimOps : Seq[PrimOp] = FirrtlPrimOpGenerator.UnOps ++ FirrtlPrimOpGenerator.BinOps ++ FirrtlPrimOpGenerator.DynShiftOps)

class FirrtlPrimOpGenerator(val settings: FirrtlPrimOpSettings, val expr: Random => Expression) extends FirrtlGenerator {

  private def binOp(rand: Random, op: PrimOp) : Expression = {
    val args = (expr(rand), expr(rand))
    val signed = (isSigned(args._1.tpe), isSigned(args._2.tpe))
    val w = Seq(getWidth(args._1.tpe), getWidth(args._2.tpe))
    val as = Seq(args._1, args._2)

    def add = DoPrim(Add, as, Seq(), signed match {
      case (false, false) => UIntType(IntWidth(max(w(0), w(1)) + 1))
      case (false, true ) => SIntType(IntWidth(max(w(0), w(1) - 1) + 2))
      case (true,  false) => SIntType(IntWidth(max(w(0) - 1, w(1)) + 2))
      case (true,  true ) => SIntType(IntWidth(max(w(0), w(1)) + 1))
    })
    def sub = DoPrim(Sub, as, Seq(), signed match {
      case (false, false) => SIntType(IntWidth(max(w(0), w(1)) + 1))
      case (false, true ) => SIntType(IntWidth(max(w(0) + 2, w(1) + 1)))
      case (true,  false) => SIntType(IntWidth(max(w(0) + 1, w(1) + 2)))
      case (true,  true ) => SIntType(IntWidth(max(w(0), w(1)) + 1))
    })
    def mul = DoPrim(Mul, as, Seq(), signed match {
      case (false, false) => UIntType(IntWidth(w(0) + w(1)))
      case _              => SIntType(IntWidth(w(0) + w(1)))
    })
    def div = DoPrim(Div, as, Seq(), signed match {
      case (false, false) => UIntType(IntWidth(w(0)))
      case (false, true ) => SIntType(IntWidth(w(0) + 1))
      case (true,  false) => SIntType(IntWidth(w(0)))
      case (true,  true ) => SIntType(IntWidth(w(0) + 1))
    })
    def rem = DoPrim(Rem, as, Seq(), signed match {
      case (false, false) => UIntType(IntWidth(min(w(0), w(1))))
      case (false, true ) => UIntType(IntWidth(min(w(0), w(1))))
      case (true,  false) => SIntType(IntWidth(min(w(0), w(1) + 1)))
      case (true,  true ) => SIntType(IntWidth(min(w(0), w(1))))
    })
    def cmp(op: PrimOp) = DoPrim(op, as, Seq(), UIntType(IntWidth(1)))
    def bit(op: PrimOp) = DoPrim(op, as, Seq(), UIntType(IntWidth(max(w(0), w(1)))))
    def cat = DoPrim(Cat, as, Seq(), UIntType(IntWidth(w(0) + w(1))))


    op match {
      case Add => add
      case Sub => sub
      case Mul => mul
      case Div => div
      case Rem => rem
      case Lt | Leq | Gt | Geq | Eq | Neq => cmp(op)
      case And | Or | Xor => bit(op)
      case Cat => cat
      case other => throw new RuntimeException(s"Unhandled prim op: $other")
    }
  }

  private def unOp(rand: Random, op: PrimOp) : Expression = {
    val arg = expr(rand)
    val (signed, w) = (isSigned(arg.tpe), getWidth(arg.tpe))

    def sameType(width: Int) = if(signed){ SIntType(IntWidth(width)) } else { UIntType(IntWidth(width)) }

    def pad = {
      val n = rand.nextInt(0, settings.MaxPad)
      DoPrim(Pad, Seq(arg), Seq(n), sameType(max(w, n)))
    }
    def asUInt = DoPrim(AsUInt, Seq(arg), Seq(), UIntType(IntWidth(w)))
    def asSInt = DoPrim(AsSInt, Seq(arg), Seq(), SIntType(IntWidth(w)))
    // TODO: as Clock
    def shiftLeft = {
      val n = rand.nextInt(0, settings.MaxShiftLeft)
      DoPrim(Shl, Seq(arg), Seq(n), sameType(n + w))
    }
    def shiftRight = {
      val over_shift = rand.nextBoolean()
      val n = if(over_shift) { rand.nextInt(w, w + 33) } else { rand.nextInt(0, w - 1) }
      DoPrim(Shr, Seq(arg), Seq(n), sameType(max(1, w - n)))
    }
    def cvt = DoPrim(Cvt, Seq(arg), Seq(), SIntType(IntWidth(if(signed){ w }else{ w + 1})))
    def neg = DoPrim(Neg, Seq(arg), Seq(), SIntType(IntWidth(w + 1)))
    def not = DoPrim(Not, Seq(arg), Seq(), UIntType(IntWidth(w)))
    def reduce(op: PrimOp) = DoPrim(op, Seq(arg), Seq(), UIntType(IntWidth(1)))
    def extract = {
      if(w > 0) {
        val lsb = rand.nextInt(0, w - 1)
        val msb = rand.nextInt(lsb, w-1)
        DoPrim(Bits, Seq(arg), Seq(msb, lsb), UIntType(IntWidth(msb - lsb + 1)))
      } else { arg }
    }
    def head = {
      if(w > 0) {
        val n = rand.nextInt(1,w)
        DoPrim(Head, Seq(arg), Seq(n), UIntType(IntWidth(n)))
      } else { arg }
    }
    def tail = {
      if(w > 0) {
        val n = rand.nextInt(0, w-1)
        DoPrim(Tail, Seq(arg), Seq(n), UIntType(IntWidth(w - n)))
      } else { arg }
    }

    op match {
      case Pad => pad
      case AsUInt => asUInt
      case AsSInt => asSInt
      case Shl => shiftLeft
      case Shr => shiftRight
      case Cvt => cvt
      case Neg => neg
      case Not => not
      case Andr | Orr | Xorr => reduce(op)
      case Bits => extract
      case Head => head
      case Tail => tail
      case other => throw new RuntimeException(s"Unhandled prim op: $other")
    }
  }

  // TODO: this is a hack that could be gotten rid off if we allow propagating type constraints
  private def getSpecificExpr(rand: Random, filter: Expression => Boolean) : Expression = {
    var e = expr(rand)
    while(!filter(e)) { e = expr(rand) }
    e
  }

  private def dynamicShift(rand: Random, op: PrimOp) : Expression = {
    val e = expr(rand)
    val (signed, w) = (isSigned(e.tpe), getWidth(e.tpe))
    def sameType(width: Int) = if(signed){ SIntType(IntWidth(width)) } else { UIntType(IntWidth(width)) }

    def shiftLeft = {
      val n = getSpecificExpr(rand, a => !isSigned(a.tpe) && getWidth(a.tpe) <= settings.MaxDynamicShiftLeftBits)
      val bits = w + (1 << getWidth(n.tpe)) - 1
      DoPrim(Dshl, Seq(e, n), Seq(), sameType(bits))
    }
    def shiftRight = {
      val n = getSpecificExpr(rand, a => !isSigned(a.tpe))
      DoPrim(Dshr, Seq(e, n), Seq(), sameType(w))
    }

    op match {
      case Dshl => shiftLeft
      case Dshr => shiftRight
      case other => throw new RuntimeException(s"Unhandled prim op: $other")
    }
  }

  private def primOp(rand: Random, op: PrimOp) : Expression = {
    op match {
      case Add | Sub | Mul | Div | Rem | Lt | Leq | Gt | Geq | Eq | Neq | And | Or | Xor | Cat => binOp(rand, op)
      case Pad | AsUInt | AsSInt | Shl | Shr | Cvt | Neg | Not | Andr | Orr | Xorr | Bits | Head | Tail => unOp(rand, op)
      case Dshl | Dshr => dynamicShift(rand, op)
      case other => throw new RuntimeException(s"Unhandled prim op: $other")
    }
  }

  def generate(rand: Random) : Expression = {
    val op = rand.choose(settings.AvailablePrimOps)
    primOp(rand, op)
  }
}



class FirrtlExprGenerator(val primOpSettings: FirrtlPrimOpSettings, val typeSettings: FirrtlTypeSettings) extends FirrtlGenerator {
  private val primOpGen = new FirrtlPrimOpGenerator(primOpSettings, expr)
  private val typeGen = new FirrtlTypeGenerator(typeSettings)

  private def literal(rand: Random, tpe: Type) : Literal = tpe match {
    case UIntType(IntWidth(w)) => UIntLiteral(rand.nextBigInteger(w.toInt), IntWidth(w))
    case SIntType(IntWidth(w)) =>
      w.toInt match {
        case 0 => SIntLiteral(0, IntWidth(0))
        case 1 => SIntLiteral(rand.choose(Seq(-1, 0)), IntWidth(1))
        case bits =>
          val absolute = rand.nextBigInteger(bits - 1)
          SIntLiteral(absolute * rand.choose(Seq(-1, 1)), IntWidth(bits))
      }
    case other => throw new RuntimeException(s"Type $other not handled.")
  }

  private def literal(rand: Random) : Literal = literal(rand, typeGen.generateGround(rand))

  private def expr(rand: Random) : Expression = {
    rand.choose(Seq(
      () => literal(rand),
      () => primOpGen.generate(rand)
    ))()
  }

  def generate(random: Random) : Expression = expr(random)
}