package app

import scalaz._
import scalaz.Functor
import scalaz.Free
import matryoshka._
import matryoshka.data._
import matryoshka.patterns.EnvT
import matryoshka.implicits._
import Scalaz._


sealed trait Flux
object Flux {
  case object Left extends Flux
  case object Right extends Flux
  case object Still extends Flux
}


case class Water(flux: Flux, volume: Int)

sealed trait BTree[A, B]
case class Leaf[A, B](index: Int, value: A) extends BTree[A, B]
//can actually annotate this with an interval range
case class Node[A, B](l: B, r: B) extends BTree[A, B]

case class Zipped[A](l: Option[A], a: A, r: Option[A])

object BTree {

  implicit def btreeRecurseFunctor[C]: Functor[BTree[C, ?]] = new Functor[BTree[C, ?]] {
    def map[A, B](tree: BTree[C, A])(f: A => B): BTree[C, B] = tree match {
      case Leaf(i, v) => Leaf(i, v)
      case Node(l, r) => Node(f(l), f(r))
    }
  }

  def build[A](depth: Int)(f: Int => A): ((Int, Int)) => BTree[A, (Int, Int)] = {
    case (d, i) => if(d < depth) {
      Node((d + 1, 2 * i - 1), (d + 1, 2 * i))
    } else {
      Leaf(i, f(i))
    }
  }

  def mapValues[A, B, T[_[_]]](f: A => B)(implicit R: CorecursiveT[T]): BTree[A, T[BTree[B, ?]]] => T[BTree[B, ?]] = {
    case Leaf(i, a) => R.embedT(Leaf(i, f(a)))
    case Node(l, r) => R.embedT(Node(l, r))
  }

  def left[A]: BTree[A, A] => A = {
    case Leaf(_, a) => a
    case Node(l, _) => l
  }

  def right[A]: BTree[A, A] => A = {
    case Leaf(_, a) => a
    case Node(_, r) => r
  }

  //apo
  def modifyLeft[A](f: A => A): Fix[BTree[A, ?]] => BTree[A, \/[Fix[BTree[A, ?]], Fix[BTree[A, ?]]]] = {
    case Fix(Leaf(i, a)) => Leaf(i, f(a))
    case Fix(Node(l, r)) => Node(\/.right(l), \/.left(r))
  }

  //apo
  def modifyRight[A](f: A => A): Fix[BTree[A, ?]] => BTree[A, \/[Fix[BTree[A, ?]], Fix[BTree[A, ?]]]] = {
    case Fix(Leaf(i, a)) => Leaf(i, f(a))
    case Fix(Node(l, r)) => Node(\/.left(l), \/.right(r))
  }

  //zygo helper
  def zipNeighboursHelper[A]: BTree[A, (A, A)] => (A, A) = {
    case Leaf(i, a) => (a, a)
    case Node((l, _), (_, r)) => (l, r) 
  }

  //zygo
  def zipNeighbours[A]: BTree[A, ((A, A), Fix[BTree[Zipped[A], ?]])] => Fix[BTree[Zipped[A], ?]] = {
    case Leaf(i, a) => Fix(Leaf(i, Zipped(None, a, None)))
    case Node(((l, lm), fixl ), ((rm, r), fixr)) => 
      val nextr = fixr.apo[Fix[BTree[Zipped[A], ?]]](modifyLeft(_.copy(l = Some(lm))))
      val nextl = fixl.apo[Fix[BTree[Zipped[A], ?]]](modifyRight(_.copy(r = Some(rm))))
      Fix(Node(nextl, nextr))
  }

  //cata
  def rivers: BTree[Water, Fix[BTree[Water, ?]]] => Fix[BTree[Water, ?]] = {
    case Leaf(i, a) => Fix(Leaf(i, a))
    case Node(l, r) =>
      val lr = l.cata(right)
      val rl = r.cata(left)
      val rnext = if(lr.flux == Flux.Right) r.apo[Fix[BTree[Water, ?]]](modifyLeft(w => w.copy(volume = w.volume + 1))) else r
      val lnext = if(rl.flux == Flux.Left) l.apo[Fix[BTree[Water, ?]]](modifyRight(w => w.copy(volume = w.volume + 1))) else l
      Fix(Node[Water, Fix[BTree[Water, ?]]](lnext, rnext))
  }
}


object Playground extends App {

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._
  import matryoshka.data.free._

  import scala.util.Random

  val random = new Random(123)

  def calcFlux(z: Zipped[Int]): Flux = z match {
    case Zipped(Some(l), a, Some(r)) => if(l <= r && l < a) Flux.Left else if(r < l && r < a) Flux.Right else Flux.Still
    case Zipped(Some(l), a, None) => if(l < a) Flux.Left else Flux.Still
    case Zipped(None, a, Some(r)) => if(r < a) Flux.Right else Flux.Still
    case _ => Flux.Still
  }

  def startWater(f: Flux): Water = Water(f, 0)

  val result = (0, 1).ana[Fix[BTree[Int, ?]]](BTree.build(2)(_ => random.nextInt(100) ))
  println(result)

  //TODO: compose catas together
  val zipped = result.zygo(BTree.zipNeighboursHelper, BTree.zipNeighbours)
  val flux = zipped.cata(BTree.mapValues[Zipped[Int], Flux, Fix](calcFlux))
  val water0 = flux.cata(BTree.mapValues[Flux, Water, Fix](startWater))
  val water1 = water0.cata(BTree.rivers)

  println(zipped)
  println(flux)
  println(water0)
  println(water1)
}
