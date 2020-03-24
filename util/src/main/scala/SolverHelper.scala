//
// SolverHelper
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//


package object solver {

  import scala.annotation.tailrec
  import scala.collection.generic.Sorted
  import scala.util.{Try, Success, Failure}
  import myutil._
  import math._
  import Instruction._

  type Register = Array[Int]
  type Assembly = Register => Unit
  type Big = BigDecimal

  @tailrec
  def generalBinarySearch[T](ineq:InEquality)(cond:Big => Int):Big = {
    if(ineq.isDot) ineq.a
    else {
      val splited = ineq.split
      cond(splited(0).b) match {
        case 0  => splited(0).b
        case 1  => generalBinarySearch(splited(1))(cond)
        case -1 => generalBinarySearch(splited(0))(cond)
      }
    }
  }

  def bfs[T](start:T)(func:T => Seq[T]):Iterator[Seq[T]] = 
    Iterator.iterate((Seq(start), Set[T](start))){case (cur, visited) => 
      val next = cur.flatMap{func}.filterNot{visited.contains}.distinct
      (next, visited ++ next)
    }.map{_._1}.takeWhile{!_.isEmpty}
 
  case class Tree[T](val value:T) {
    private var children:Set[Tree[T]] = Set()
    def ->(value:Tree[T]) = this.children += value
    override def toString = {
      (for{
        layer <- bfs(this)(_.children.toSeq)
      } yield layer.map{_.value}.mkString("{", ",", "}")).mkString("\n")
    }
  }

  case class Node[T](val value:T) {
    private var _neighbors:Set[Node[T]] = Set()
    def neighbors = _neighbors.toSeq
    def --(other:Node[T]) = {
      this._neighbors += other
      other._neighbors += this
    }
  }

  object Instruction {
    type InstructSet = Set[Instruct]

    abstract class Instruct(func:(Int, Int, Int, Register) => Int, pattern:String) {
      def execute(a:Int, b:Int, c:Int, reg:Register) { reg(c) = func(a, b, c, reg) }
    }

    case object Addr extends Instruct((a, b, c, reg) => reg(a) + reg(b), "addr")
    case object Addi extends Instruct((a, b, c, reg) => reg(a) + b, "addi")
    case object Mulr extends Instruct ((a, b, c, reg) => reg(a) * reg(b), "mulr")
    case object Muli extends Instruct ((a, b, c, reg) => reg(a) * b, "muli")
    case object Banr extends Instruct ((a, b, c, reg) => reg(a) & reg(b), "banr")
    case object Bani extends Instruct ((a, b, c, reg) => reg(a) & b, "bani")
    case object Borr extends Instruct ((a, b, c, reg) => reg(a) | reg(b), "borr")
    case object Bori extends Instruct ((a, b, c, reg) => reg(a) | b, "bori")
    case object Setr extends Instruct ((a, b, c, reg) => reg(a), "setr")
    case object Seti extends Instruct ((a, b, c, reg) => a ,"seti")
    case object Gtir extends Instruct ((a, b, c, reg) => if(a > reg(b)) 1 else 0, "gtir")
    case object Gtri extends Instruct ((a, b, c, reg) => if(reg(a) > b) 1 else 0, "gtri")
    case object Gtrr extends Instruct ((a, b, c, reg) => if(reg(a) > reg(b)) 1 else 0, "gtrr")
    case object Eqir extends Instruct ((a, b, c, reg) => if(a == reg(b)) 1 else 0, "eqir")
    case object Eqri extends Instruct ((a, b, c, reg) => if(reg(a) == b) 1 else 0, "eqri")
    case object Eqrr extends Instruct ((a, b, c, reg) => if(reg(a) == reg(b)) 1 else 0, "eqrr")
  }

  var instMap:(Int, Int, Int) => Map[String, Assembly] = 
  (a, b, c) => Map[String, Assembly]( 
      "addi" -> ((reg:Register) => Addi.execute(a, b, c, reg)),
      "addr" -> ((reg:Register) => Addr.execute(a, b, c, reg)),
      "mulr" -> ((reg:Register) => Mulr.execute(a, b, c, reg)),
      "muli" -> ((reg:Register) => Muli.execute(a, b, c, reg)),
      "banr" -> ((reg:Register) => Banr.execute(a, b, c, reg)),
      "bani" -> ((reg:Register) => Bani.execute(a, b, c, reg)),
      "borr" -> ((reg:Register) => Borr.execute(a, b, c, reg)),
      "bori" -> ((reg:Register) => Bori.execute(a, b, c, reg)),
      "setr" -> ((reg:Register) => Setr.execute(a, b, c, reg)),
      "seti" -> ((reg:Register) => Seti.execute(a, b, c, reg)),
      "gtir" -> ((reg:Register) => Gtir.execute(a, b, c, reg)),
      "gtri" -> ((reg:Register) => Gtri.execute(a, b, c, reg)),
      "gtrr" -> ((reg:Register) => Gtrr.execute(a, b, c, reg)),
      "eqir" -> ((reg:Register) => Eqir.execute(a, b, c, reg)),
      "eqri" -> ((reg:Register) => Eqri.execute(a, b, c, reg)),
      "eqrr" -> ((reg:Register) => Eqrr.execute(a, b, c, reg))
  )
  def inputToAssembly(input:Seq[String], map:(Int, Int, Int) => Map[String, Assembly] = instMap):Array[Assembly] = {
    lazy val pattern = """([a-z]+) ([0-9]+) ([0-9]+) ([0-9]+)""".r.anchored
    input.map {
      case pattern(instruct, aStr, bStr, cStr) => 
        map(aStr.toInt, bStr.toInt, cStr.toInt)(instruct)
    }.toArray
  }

  def instructExecuteStream(instSet:Seq[Assembly], instPntIdx:Int, register:Register):Stream[Register] = {
    lazy val stream:Stream[Register] = (register) #:: stream.map { (reg) => 
        instSet(reg(instPntIdx))(reg) 
        reg(instPntIdx) += 1
        reg.clone()
      }
    stream.takeWhile{x=> x(instPntIdx) < instSet.size}
  }


  implicit class ValString(val str:String) extends AnyVal {
    def takeInt = str.takeWhile(_.isDigit).toInt
  }

  case class Area(val lt:Pos, val rb:Pos) {
    def points = for{
      x <- (lt.x to rb.x).toIterator
      y <- (lt.y to rb.y)
    } yield Pos(x, y)

    def extend(amount:Int) = Area(lt - Pos(amount, amount), rb + Pos(amount, amount))
    def printString(func:(Int, Int) => Char):String = {
      (lt.y to rb.y).map{ y =>
        (lt.x to rb.x).map{ x => 
          func(x, y)
        }.mkString("")
      }.mkString("\n")
    }
  }
  object Area {
    def minMaxArea(points:List[Pos]) = {
      def getMinMax(func:Pos=>Int) =
        (func(points.minBy(func)),func(points.maxBy(func)))
      lazy val (minX, maxX) = getMinMax(_.x)
      lazy val (minY, maxY) = getMinMax(_.y)
      Area(Pos(minX, minY), Pos(maxX, maxY))
    }
  }
  implicit class IntUtil(val i:Int) extends AnyVal {
    def times(t: => Unit) {
      (1 to i).foreach{i=>t}
    }
    def foldLeft[T](first:T)(t: (T,Int) => T):T = {
      (1 to i).foldLeft(first){t}
    }
  }
  case class Pos(val x:Int, val y:Int) extends Ordered[Pos] {
    def this(arr:Array[String]) = this(arr(0).toInt, arr(1).toInt)
    def +(other:Pos) = Pos(x + other.x, y + other.y)
    def -(other:Pos) = Pos(x - other.x, y - other.y)
    def *(value:Int) = Pos(x * value, y * value)
    def /(value:Int) = Pos(x / value, y / value)
    def manhattan(that:Pos) = (this.x - that.x).abs + (this.y - that.y).abs
    def dist(that:Pos) = {
      val distX = this.x - that.x
      val distY = this.y - that.y
      sqrt(distX*distX + distY*distY)
    }
    def ordering = Ordering.by[Pos, (Int, Int)]{case Pos(x, y) => (y, x)}
    def compare(that:Pos) = ordering.compare(this, that)
    def up    = this + Pos(0, -1)
    def down  = this + Pos(0, 1)
    def left  = this + Pos(-1, 0)
    def right = this + Pos(1, 0)
    def nears:List[Pos] = List(up, left, right, down)
    def adjacent:Seq[Pos] = {
      for{
        otherX <- (-1 to 1)
        otherY <- (-1 to 1)
        if otherX != 0 && otherY != 0
      } yield Pos(x+otherX, y+otherY)
    }
  }

  class Dynamic[T,V](func:(T, Dynamic[T, V]) => V) {
    import collection.mutable.Map
    private var memoization:Map[T, V] = Map()
    def apply(i:T):V = memoization.getOrElseUpdate(i, func(i, this))
    def mapValue[A](argFunc:V => A):Dynamic[T, A] = {
      val parent = this
      val newFunc = { (i:T, prev:Dynamic[T, A]) => argFunc(parent(i)) }
      new Dynamic[T, A](newFunc)
    }
  }

  case class Pos3D(x:Big, y:Big, z:Big) {
    import math._
    def |-|(other:Pos3D)    = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
    def +(other:Pos3D)      = Pos3D(x + other.x, y + other.y, z + other.z)
    def *(value:Big)        = Pos3D(x * value, y * value, z * value)
    def *(value:Int):Pos3D  = *(BigDecimal(value))
    def manhattanOrigin:Big = |-|(Pos3D(0, 0, 0))
  }

  case class InEquality(a:Big, b:Big) {
    require(a <= b)
    def split = {
      if(a == b) List(this)
      else {
        def floor = BigDecimal.RoundingMode.FLOOR
        val range = b - a 
        val a1 = (a + range / 2.0).setScale(0, floor)
        List(InEquality(a, a1), InEquality(a1+1, b))
      }
    }
    def isDot:Boolean = a == b
  }

  case class Box(min:Pos3D, max:Pos3D) {
    require(min.x <= max.x && min.y <= max.y && min.z <= max.z, s"${min} ${max}")
    def isDot = (min.x == max.x) && min.y == max.y && min.z == max.z
    private def closeAxis(pos:Pos3D)(f:Pos3D => Big) = {
      val value = f(pos)
      if(value < f(min)) f(min) else
      if(value > f(max)) f(max) else value
    }
    private def closestPos(pos:Pos3D) = Pos3D(closeAxis(pos)(_.x),
      closeAxis(pos)(_.y), closeAxis(pos)(_.z))
    def split = {
      if(isDot) Nil else {
        for {
          x <- InEquality(min.x, max.x).split
          y <- InEquality(min.y, max.y).split
          z <- InEquality(min.z, max.z).split
        } yield Box(Pos3D(x.a, y.a, z.a), Pos3D(x.b, y.b, z.b))
      }
    }
  }

}
