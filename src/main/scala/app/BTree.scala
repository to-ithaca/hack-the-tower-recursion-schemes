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

  def setLeft[A](a: A): Either[Fix[BTree[Zipped[A], ?]], Fix[BTree[Zipped[A], ?]]] => BTree[Zipped[A], Either[Fix[BTree[Zipped[A], ?]], Fix[BTree[Zipped[A], ?]]]] = {
    case Left(Fix(Leaf(i, z))) => Leaf(i, z.copy(l = Some(a)))
    case Left(Fix(Node(l, r))) => Node(Left(l), Right(r))
    case Right(Fix(Leaf(i, z))) => Leaf(i, z)
    case Right(Fix(Node(l, r))) => Node(Right(l), Right(r))
  }


  def modifyLeft[A](f: A => A): Either[Fix[BTree[A, ?]], Fix[BTree[A, ?]]] => BTree[A, Either[Fix[BTree[A, ?]], Fix[BTree[A, ?]]]] = {
    case Left(Fix(Leaf(i, z))) => Leaf(i, f(z))
    case Left(Fix(Node(l, r))) => Node(Left(l), Right(r))
    case Right(Fix(Leaf(i, z))) => Leaf(i, z)
    case Right(Fix(Node(l, r))) => Node(Right(l), Right(r))
  }

  def setRight[A](a: A): Either[Fix[BTree[Zipped[A], ?]], Fix[BTree[Zipped[A], ?]]] => BTree[Zipped[A], Either[Fix[BTree[Zipped[A], ?]], Fix[BTree[Zipped[A], ?]]]] = {
    case Left(Fix(Leaf(i, z))) => Leaf(i, z)
    case Left(Fix(Node(l, r))) => Node(Left(l), Left(r))
    case Right(Fix(Leaf(i, z))) => Leaf(i, z.copy(r = Some(a)))
    case Right(Fix(Node(l, r))) => Node(Left(l), Right(r))
  }


  def modifyRight[A](f: A => A): Either[Fix[BTree[A, ?]], Fix[BTree[A, ?]]] => BTree[A, Either[Fix[BTree[A, ?]], Fix[BTree[A, ?]]]] = {
    case Left(Fix(Leaf(i, z))) => Leaf(i, z)
    case Left(Fix(Node(l, r))) => Node(Left(l), Left(r))
    case Right(Fix(Leaf(i, z))) => Leaf(i, f(z))
    case Right(Fix(Node(l, r))) => Node(Left(l), Right(r))
  }

  case class Zipped[A](l: Option[A], a: A, r: Option[A])

  //a basic catamorphism
  def zipNeighbours[A]: BTree[A, Fix[BTree[Zipped[A], ?]]] => Fix[BTree[Zipped[A], ?]] = {
    case Leaf(i, a) => Fix(Leaf(i, Zipped(None, a, None)))
    case Node(l, r) => // we want to set l's right to r and r's left to l
      val lr = l.cata(right)
      val rl = r.cata(left)
      println(s"values $lr $rl")
      val leftr: Either[Fix[BTree[Zipped[A], ?]], Fix[BTree[Zipped[A], ?]]] = Left(r)
      val rightl: Either[Fix[BTree[Zipped[A], ?]], Fix[BTree[Zipped[A], ?]]] = Right(l)
      val lana = leftr.ana[Fix[BTree[Zipped[A], ?]]](setLeft(lr.a))
      val rana = rightl.ana[Fix[BTree[Zipped[A], ?]]](setRight(rl.a))
      println(s"fixes $lana $rana")
      Fix(Node[Zipped[A], Fix[BTree[Zipped[A], ?]]](rana, lana))
  }

  def rivers: BTree[Water, Fix[BTree[Water, ?]]] => Fix[BTree[Water, ?]] = {
    case Leaf(i, a) => Fix(Leaf(i, a))
    case Node(l, r) => // we want to set l's right to r and r's left to l
      val lr = l.cata(right)
      val rl = r.cata(left)
      val leftr: Either[Fix[BTree[Water, ?]], Fix[BTree[Water, ?]]] = Left(r)
      val rightl: Either[Fix[BTree[Water, ?]], Fix[BTree[Water, ?]]] = Right(l)
      val rnext = if(lr.flux == Flux.Right) leftr.ana[Fix[BTree[Water, ?]]](modifyLeft(w => w.copy(volume = w.volume + 1))) else r
      val lnext = if(rl.flux == Flux.Left) rightl.ana[Fix[BTree[Water, ?]]](modifyRight(w => w.copy(volume = w.volume + 1))) else l
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

  def calcFlux(z: BTree.Zipped[Int]): Flux = z match {
    case BTree.Zipped(Some(l), a, Some(r)) => if(l <= r && l < a) Flux.Left else if(r < l && r < a) Flux.Right else Flux.Still
    case BTree.Zipped(Some(l), a, None) => if(l < a) Flux.Left else Flux.Still
    case BTree.Zipped(None, a, Some(r)) => if(r < a) Flux.Right else Flux.Still
    case _ => Flux.Still
  }

  def startWater(f: Flux): Water = Water(f, 0)

  val result = (0, 1).ana[Fix[BTree[Int, ?]]](BTree.build(2)(_ => random.nextInt(100) ))
  println(result)

  val zipped: Fix[BTree[BTree.Zipped[Int], ?]] = result.cata(BTree.zipNeighbours)
  val flux = zipped.cata(BTree.mapValues[BTree.Zipped[Int], Flux, Fix](calcFlux))
  val water0 = flux.cata(BTree.mapValues[Flux, Water, Fix](startWater))
  val water1 = water0.cata(BTree.rivers)

  println(zipped)
  println(flux)
  println(water0)
  println(water1)
}
