/// Imports
import java.awt.{ Graphics2D, Color }
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.collection.mutable._
import event._
import scala.math._

/// Main
object Main extends SimpleSwingApplication {
  // Displays the canvas
  var myCanvas: Option[Canvas] = None
  var w: Int = 1200
  var h: Int = 800
  
  def top = new MainFrame {
    val canvas = new Canvas {
      preferredSize = new Dimension(w, h)
    }
    myCanvas = Some(canvas)
    title = "Appolonian Circle Packings"
    contents = canvas
    size = new Dimension(w, h)
    
    Program.init
  }
}

/// Canvas
class Canvas extends Panel {
  var s = Scale()
  //// Read Keys
  focusable = true
  listenTo(mouse.clicks)
  listenTo(keys)
  reactions += {
    case KeyTyped(_, key, _, _) => {
      key match {
        case ' ' => Program.triple.recurse
        case '=' | '+' => s = Scale(s.s * 1.2, s.x, s.y)
        case '-' | '_' => s = Scale(s.s / 1.2, s.x, s.y)
        case 'h' => s = Scale(s.s, s.x + 10, s.y)
        case 'l' => s = Scale(s.s, s.x - 10, s.y)
        case 'j' => s = Scale(s.s, s.x, s.y - 10)
        case 'k' => s = Scale(s.s, s.x, s.y + 10)
        case 'H' => s = Scale(s.s, s.x + 40, s.y)
        case 'L' => s = Scale(s.s, s.x - 40, s.y)
        case 'J' => s = Scale(s.s, s.x, s.y - 40)
        case 'K' => s = Scale(s.s, s.x, s.y + 40)
        case _ => println("Wrong key! " + key)
      }
      repaint
    }
  }
  
  //// Paint the Canvas
  override def paintComponent(g: Graphics2D) {
    // Start by erasing this Canvas
    g.setColor(Color.white)
    g.clearRect(0, 0, size.width, size.height)
    g.fillRect(0, 0, size.width, size.height)
    
    g.setColor(Color.blue)
    Program.triple.draw(g, s, self = true)
  }
}

/// Scale coordinates
case class Scale (val s: Double = 1, val x: Int = 0, val y: Int = 0,
  val w: Double = Double.PositiveInfinity, val h: Double = Double.PositiveInfinity) {
  def xCoord(initial: Double): Int = (x + initial * s).toInt
  def yCoord(initial: Double): Int = (y + initial * s).toInt
}

/// Program Object (Heavy Lifting)
object Program {
  //// Initialization
  def init = () // Called from main class to initialize some things
  println("Initializing!!")
  
  val maxCurvature = 1.0
  val startR = 100
  val startPos = (500.0, 140.0)
  var r1 = 0.5
  var r2 = 0.5
  var triple = filledTriple(r1, r2, startR, startPos)

  def filledTriple(r1: Double, r2: Double, startR: Double, startPos: (Double, Double)): Triple = {
    val c1 = Circle(1 / startR, startPos._1, startPos._2)
    val c2 = Circle(1 / (startR * r1), startPos._1 + startR + (startR * r1), startPos._2)
    val c3 = tangentsCC(c1, c2, 1 / (startR * r1 * r2))._1
    val t = Triple(c1, c2, c3)
    t.recurse
    return t
  }

  //// Beurocratic Mumbo Jumbo- - - - - - - - - - -
  //// New Apollone LLC
  def newApolloneLLC: Option[Apollone] = None
  
  //// New Apollone LCC
  def newApolloneLCC: Option[Apollone] = None
  
  //// New Apollone CCC
  def newApolloneCCC(c1: Circle, c2: Circle, c3: Circle, out: Boolean): Option[Apollone] = {
    // The curvature and radius of the circle that would fit between c1, c2, and c3
    val newK = Program.newCurvature(c1.k, c2.k, c3.k, out)
    val newR = 1.0 / abs(newK)
    
    // The two possible circles that are adjacent to c1 and c2
    val cands = tangentsCC(c2, c3, newK)
    
    // Which circle is closer to the existing circle
    def circleError(candidate: Circle): Double =
      abs(circleDistance(c1, candidate)
        + {if (c1.k > 0) - newR else newR}
        + {if (newK > 0) - c1.r else c1.r})
    
    if (circleError(cands._1) < circleError(cands._2))
      Some(cands._1) else Some(cands._2)
  }
  
  //// Math Mumbo Jumbo - - - - - - - - - - - - - -
  //// New Curvature
  def newCurvature(a: Double, b: Double, c: Double, out: Boolean): Double =
    if (out) outsideCurvature(a, b, c) else insideCurvature(a, b, c)
  
  def insideCurvature(a: Double, b: Double, c: Double): Double =
    a + b + c + 2 * sqrt(a*b + a*c + b*c)
  
  def outsideCurvature(a: Double, b: Double, c: Double): Double =
    a + b + c - 2 * sqrt(a*b + a*c + b*c)
  
  //// Tangent to Circles
  def tangentsCC(c1: Circle, c2: Circle, k: Double): (Circle, Circle) = {
    val r = 1.0 / abs(k)
    
    // The distances from the center of c1/c2 to the center of the new circle
    val d1 = if (k > 0) r + c1.r else r - c1.r
    val d2 = if (k > 0) r + c2.r else r - c2.r
    
    // The x and y differences between the centers of c1 and c2
    val deltaX = c2.x - c1.x
    val deltaY = c2.y - c1.y
    
    // The distance between c1 and c2
    val b = sqrt(deltaX*deltaX + deltaY*deltaY)
    
    // The centers of c1, c2, and each of the new circles form two congruent triangles
    // The length altitude from the center of the new circle of each of these triangles is h
    // The distance from the center of c1 to the intersection of the altitude to line c1 c2 is b1
    val b1 = (d1*d1 - d2*d2 + b*b) / (2 * b)
    val h = sqrt(d1*d1 - b1*b1)
    
    // The intersections of the line from c1 to c2, and the perpendicular lines coming from the new circles
    val intersectX = c1.x + b1/b * deltaX
    val intersectY = c1.y + b1/b * deltaY
    
    // The centers of the two adjacent circles are (new1X, new1Y) and (new2X, new2Y)
    val new1X = intersectX + h/b * deltaY
    val new1Y = intersectY - h/b * deltaX
    val new2X = intersectX - h/b * deltaY
    val new2Y = intersectY + h/b * deltaX
    
    (new Circle(k, new1X, new1Y), new Circle(k, new2X, new2Y))
  }
  
  //// Tangent to Circle and Line
  //def tangentsLC(l: Line, c: Circle, k: Double): (Circle, Circle) = {
  //}

  //// Circle Distance
  def circleDistance(c1: Circle, c2: Circle): Double = sqrt(pow(c2.x-c1.x, 2) + pow(c2.y-c1.y, 2))
  def fartherCircle(old: Circle, cand1: Circle, cand2: Circle): Circle =
    if (circleDistance(old, cand1) < circleDistance(old, cand2)) cand2 else cand1
  
  def otherApollone(old: Apollone, cand1: Apollone, cand2: Apollone): Apollone =
    (old, cand1, cand2) match {
      case (cOld: Circle, c1: Circle, c2: Circle) => fartherCircle(cOld, c1, c2)
      case (lOld: Line, l1: Line, l2: Line) =>
        if (abs(lOld.b - l1.b) < abs(lOld.b - l2.b)) cand2 else cand1
      case (lOld: Line, l1: Line, c2: Circle) => l1
      case (cOld: Circle, c1: Circle, l2: Line) => c1
      case _ => cand2
    }
}

/// Apollonian Classes
sealed trait Apollone {
  def k: Double // Curvature of the object
  def draw(g: Graphics2D, scale: Scale = Scale()): Unit
}

case class Circle(val k: Double, val x: Double, val y: Double) extends Apollone {
  val r = 1.0 / abs(k)
  val diameter = 2.0 * r
  def surrounding = k < 0
  def draw(g: Graphics2D, s: Scale = Scale()) =
    g.drawOval(s.xCoord(x - r), s.yCoord(y - r), (diameter * s.s).toInt, (diameter * s.s).toInt)
  override def toString = f"Circle{k=$k r=$r,($x,$y)}"
}

case class Line(val m: Double, val b: Double) extends Apollone {
  def k = 0
  def draw(g: Graphics2D, s: Scale = Scale()) = if (m == Double.PositiveInfinity)
    g.drawLine(s.xCoord(b), s.yCoord(0), s.xCoord(b), s.yCoord(Main.h))
  else
    g.drawLine(s.xCoord(0), s.yCoord(b), s.xCoord(Main.w), s.yCoord(Main.w * m + b))
  override def toString = f"Line{y=${m}x+$b}"
}

/// Extremas
class Extremas2D(val minX: Double, val maxX: Double, val minY: Double, val maxY: Double) {
  def newExtremas(minX2: Double, maxX2: Double, minY2: Double, maxY2: Double): Extremas2D =
    if (minX >= minX2 && maxX >= maxX2 && minY >= minY2 && maxY >= maxY2) this
    else new Extremas2D(minX.min(minX2), maxX.max(maxX2), minY.min(minY2), maxY.max(maxY2))
  override def toString = f"Extremas2D($minX,$maxX)($minY,$minX)"
}

object Extremas2D {
  def apply(minX: Double, maxX: Double, minY: Double, maxY: Double) =
    new Extremas2D(minX, maxX, minY, maxY)
  
  val infinite = Extremas2D(Double.NegativeInfinity, Double.PositiveInfinity, Double.NegativeInfinity, Double.PositiveInfinity)
  
  def compose(list: List[Extremas2D]): Extremas2D = list match {
    case List(x) => x
    case x::xs => compose(xs).newExtremas(x.minX, x.maxX, x.minY, x.maxY)
    case Nil => infinite
  }
}

/// Triple Class
case class Triple(_a1: Apollone, _a2: Apollone, _a3: Apollone) {
  //// Initialization
  // a1 has the lowest curvature (largest) and a3 has the highest curvature (smallest)
  val sorted: List[Apollone] = List(_a1, _a2, _a3).sortWith(_.k < _.k)
  val a1 = sorted(0)
  val a2 = sorted(1)
  val a3 = sorted(2)
  
  override def toString = f"{${a1.k}, ${a2.k}, ${a3.k}}"
  
  //// Inside and outside variables
  private var _iT: Option[(Triple, Triple, Triple)] = None
  def iT = _iT
  private var _in: Option[Apollone] = None
  def in = _in
  
  private var _oT: Option[(Triple, Triple, Triple)] = None
  def oT = _oT
  private var _out: Option[Apollone] = None
  def out = _out
  
  //// Setting inside and outside
  def setIn(a: Apollone, t1: Triple, t2: Triple, t3: Triple): (Triple, Triple, Triple) = {
    if (_in.isEmpty) {
      _in = Some(a)
      _iT = Some(Tuple3(t1, t2, t3))
    }
    return _iT.get
  }
  def setIn(a: Apollone): (Triple, Triple, Triple) =
    setIn(a, Triple(a, a2, a3), Triple(a, a1, a3), Triple(a, a1, a2))
  
  def setOut(a: Apollone, t1: Triple, t2: Triple, t3: Triple): (Triple, Triple, Triple) = {
    if (_out.isEmpty) {
      _out = Some(a)
      _oT = Some(Tuple3(t1, t2, t3))
    }
    return _oT.get
  }
  def setOut(a: Apollone): (Triple, Triple, Triple) =
    setOut(a, Triple(a, a2, a3), Triple(a, a1, a3), Triple(a, a1, a2))
  
  //// Drawing
  def draw(g: Graphics2D, s: Scale = Scale(), from: Option[Triple] = None, self: Boolean = false): Unit = {
    if (self) List(a1, a2, a3).foreach(_.draw(g, s))
    
    // Draw the inside and outside circles if they aren't part of the from triple
    for (maybeApollone <- List(in, out))
      maybeApollone match {
        case Some(a) =>
          from match {
            case Some(f) => if (a != f.a1 && a != f.a2 && a != f.a3) a.draw(g, s)
            case None => a.draw(g, s)
          }
        case None =>
      }
    
    // Recursively draw the inside and outside triples if they aren't the from triple
    for (maybeTripleTuple <- List(iT, oT))
      maybeTripleTuple match {
        case Some((i1, i2, i3)) =>
          from match {
            case Some(f) => if (i1 != f && i2 != f && i3 != f)
              List(i1, i2, i3).foreach(_.draw(g, s, from = Some(this)))
            case None => List(i1, i2, i3).foreach(_.draw(g, s, from = Some(this)))
          }
        case None =>
      }
  }
  
  //// Recursion
  def recurse: Unit = if (in.isEmpty || out.isEmpty) {
    (a1, a2, a3) match {
      case (c1: Circle, c2: Circle, c3: Circle) => {
        if (a1.k < 0 && (in.isDefined ^ out.isDefined)) {
          val cands: List[Option[Apollone]] = List(true, false).map(Program.newApolloneCCC(c1, c2, c3, _))
          val result = Program.otherApollone(if (in.isDefined) in.get else out.get, cands(0).get, cands(1).get)
          recurseWith(result, out.isEmpty)
        } else {
          for (inOrOut <- List((out, true), (in, false)))
            if (inOrOut._1.isEmpty) Program.newApolloneCCC(c1, c2, c3, inOrOut._2) match {
              case Some(a) => recurseWith(a, inOrOut._2)
              case None =>
            }
        }
      }
      case _ =>
    }
  }
  
  def recurseWith(a: Apollone, outside: Boolean): Unit =
    if (a.k < Program.maxCurvature) {
      val t1 = Triple(a, a2, a3)
      val t2 = Triple(a, a1, a3)
      val t3 = Triple(a, a1, a2)
      if (outside) {
        // Since the in/out for triples involving surrounding circles is irrelevant,
        // don't bother with an extra conditional for it
        this.setOut(a, t1, t2, t3)
        t1.setOut(a1, this, t2, t3)
        t2.setOut(a2, this, t1, t3)
        t3.setIn(a3, this, t1, t2)
      } else { // If the new circle is on the inside
        this.setIn(a, t1, t2, t3)
        t1.setOut(a1, this, t2, t3)
        t2.setOut(a2, this, t1, t3)
        t3.setOut(a3, this, t1, t2)
      }
      List(t1, t2, t3).foreach(_.recurse)
    }
}
