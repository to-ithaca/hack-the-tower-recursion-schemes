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
case class River(path: List[Int], active: Boolean) {
  def addPoint(i: Int): River = copy(path = i :: path)
}
case class RiverTile(flux: Flux, rivers: List[River]) {
  def markInactive: RiverTile = copy(rivers = rivers.map(_.copy(active = false)))
  def addRivers(rs: List[River]): RiverTile = copy(rivers = rs ::: rivers)
}

case class Point(x: Int, y: Int)

sealed trait BTree[A, B]
case class Leaf[A, B](index: Point, value: A) extends BTree[A, B]
//can actually annotate this with an interval range
case class Node[A, B]tl: B, tr: B, bl: B, br: B) extends BTree[A, B]

case class Zipped[A](l: Option[A], a: A, r: Option[A])

//er could make a recursive structure for this ... 
case Edges[A](l: List[A], r: List[A], t: List[A], b: List[A])



object BTree {

  implicit def btreeRecurseFunctor[C]: Functor[BTree[C, ?]] = new Functor[BTree[C, ?]] {
    def map[A, B](tree: BTree[C, A])(f: A => B): BTree[C, B] = tree match {
      case Leaf(i, v) => Leaf(i, v)
      case Node(tl, tr, bl, br) => Node(f(tl), f(tr), f(bl), f(br))
    }
  }

  def build[A](depth: Int)(f: Point => A): ((Int, Point)) => BTree[A, (Int, Point)] = {
    case (d, p) => if(d < depth) {
      val d1 = d + 1
      vsl bl = Point(2 * p.x - 1, 2 * p.y -  1)
      vsl tl = Point(2 * p.x - 1, 2 * p.y)
      vsl br = Point(2 * p.x, 2 * p.y -  1)
      vsl tr = Point(2 * p.x, 2 * p.y)
      Node((d1, tl), (d1, tr), (d1, bl), (d1, br))
    } else {
      Leaf(p, f(p))
    }
  }

  def mapValues[A, B, T[_[_]]](f: A => B)(implicit R: CorecursiveT[T]): BTree[A, T[BTree[B, ?]]] => T[BTree[B, ?]] = {
    case Leaf(i, a) => R.embedT(Leaf(i, f(a)))
    case Node(tl, tr, bl, br) => R.embedT(Node(tl, tr, bl, br))
  }

  def edgesHelper[A]: BTree[A, Edges[A]] => Edges[A] = {
    case Leaf(i, a) => Edges(a, a, a, a)
    case Node(tl, tr, bl, br) => Edges(tl.l ++ bl.l, tr.r ++ br.r, tl.t ++ tr.t, bl.b ++ br.b)
  }

  def zipNeighboursHelper[A]: BTree[A, (A, A)] => (A, A) = {
    case Leaf(i, a) => (a, a)
    case Node((l, _), (_, r)) => (l, r) 
  }

  def left[A]: BTree[A, A] => A = {
    case Leaf(_, a) => a
    case Node(l, _) => l
  }


  def leftIndex[A]: BTree[A, (Int, A)] => (Int, A) = {
    case Leaf(i, a) => (i, a)
    case Node(l, _) => l
  }

  def rightIndex[A]: BTree[A, (Int, A)] => (Int, A) = {
    case Leaf(i, a) => (i, a)
    case Node(_, r) => r
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


  def shouldSeed(i: Int): Boolean = i % 2 == 0

  def seedRivers: BTree[Flux, ?] ~> BTree[RiverTile, ?] =
    new (BTree[Flux, ?] ~> BTree[RiverTile, ?]) {
      def apply[A](tree: BTree[Flux, A]): BTree[RiverTile, A] = tree match {
        case Leaf(i, flux) =>
          if(shouldSeed(i)) Leaf(i, RiverTile(flux, List(River(i :: Nil, true))))
          else Leaf(i, RiverTile(flux, Nil))
        case Node(l, r) => Node(l, r)
      }
    }

  def flowRivers: BTree[RiverTile, Fix[BTree[RiverTile, ?]]] => Fix[BTree[RiverTile, ?]] = {
    case Leaf(i, a) => Fix(Leaf(i, a))
    case Node(l, r) =>
      val (li, lr) = l.cata(rightIndex)
      val (ri, rl) = r.cata(leftIndex)

      val lrivers = lr.rivers.filter(_.active).map(_.addPoint(li))
      val (nextL, nextR) = if(lrivers.nonEmpty && lr.flux == Flux.Right) {

        //remove it from this one
        val nextL = l.apo[Fix[BTree[RiverTile, ?]]](modifyRight(tile => tile.markInactive))
        //add it to this one
        val nextR = r.apo[Fix[BTree[RiverTile, ?]]](modifyLeft(tile => tile.addRivers(lrivers)))
        (nextL, nextR.cata(flowRivers)) // reflow with inside
      } else (l, r)

      val rrivers = rl.rivers.filter(_.active).map(_.addPoint(li))
      val (nextL1, nextR1) = if(rrivers.nonEmpty && rl.flux == Flux.Left) {

        //remove it from this one
        val nextR0 = nextR.apo[Fix[BTree[RiverTile, ?]]](modifyLeft(tile => tile.markInactive))
        //add it to this one
        val nextL0 = nextL.apo[Fix[BTree[RiverTile, ?]]](modifyRight(tile => tile.addRivers(rrivers)))
        (nextL0, nextR0.cata(flowRivers))
      } else (nextL, nextR)

      Fix(Node(nextL1, nextR1))
  }
  //when we encounter a node
  //look at the edges of each subnode
  //for the internal edges
  //advect them to other nodes
  //add them to the nodes
  //and then repeat
  
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

  val zipped = result.zygo(BTree.zipNeighboursHelper, BTree.zipNeighbours)
  val flux = zipped.cata(BTree.mapValues[Zipped[Int], Flux, Fix](calcFlux))
  val seeded = flux.cata[Fix[BTree[RiverTile, ?]]](fix => Fix(BTree.seedRivers[Fix[BTree[RiverTile, ?]]](fix)))
  val flow = seeded.cata(BTree.flowRivers)

  println(zipped)
  println(flux)
  println(seeded)
  println(flow)
}

/*

 Would be good to come up with a similar approach to Conway's game of life by building the tree one level down
 So at each level, you could confiden


 So if we look at a 4 by 4 square... we can say that the inner parts of the 4 by 4 square are affected

 x x x x
 x x x x
 x x x x
 x x x x

 Maybe this just doesn't scale
 
 */
