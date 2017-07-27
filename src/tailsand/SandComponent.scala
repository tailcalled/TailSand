package tailsand

import javax.swing.JComponent
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Color
import scala.collection.mutable.ArrayBuffer
import java.awt.image.BufferedImage
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.util.concurrent.ConcurrentLinkedQueue
import java.awt.event.MouseMotionListener
import java.util.Random

class SandComponent[P <: ParticleType](w: Int, h: Int, mod: SandMod { type Particle = P }) extends JComponent {
  
  setMinimumSize(new Dimension(3 * w, 3 * h))
  setPreferredSize(new Dimension(3 * w, 3 * h))
  
  def makeColor(col: (Int, Int, Int)) = new Color(col._1, col._2, col._3)
  private val back = makeColor(mod.empty.color)
  
  private val state = new GridBuffer[P](w, h)
  private val rnd = new Random()
  
  private var img = new BufferedImage(3 * w, 3 * h, BufferedImage.TYPE_INT_ARGB)
  
  var brush: P = null.asInstanceOf[P]
  var brushSize = 3
  private val paintQueue = new ConcurrentLinkedQueue[PaintAction]()
  
  var prevX = 0
  var prevY = 0
  
  sealed trait PaintAction {
    val x: Int
    val y: Int
  }
  case class InitAction(x: Int, y: Int, brush: P, size: Int) extends PaintAction
  case class DragAction(x: Int, y: Int, brush: P, size: Int) extends PaintAction
  
  private def place(brush: P, x: Int, y: Int) = {
    if (x < 0 || y < 0 || x >= w || y >= h) ()
    else {
      if (state(x, y) != null) {
        state.delete(state(x, y))
      }
      if (brush != mod.empty) {
        state.create(brush, x, y)
      }
      ()
    }
  }
  private def circ(brush: P, x: Int, y: Int, rad: Int) = {
    for (dx <- -rad to rad; dy <- -rad to rad; if dx*dx + dy*dy <= rad * rad) {
      place(brush, x + dx, y + dy)
    }
  }
  def input() = {
    var action: PaintAction = null
    while ({ action = paintQueue.poll(); action != null }) {
      action match {
        case InitAction(x, y, brush, size) =>
          circ(brush, x, y, size)
        case DragAction(x, y, brush, size) =>
          if (x != prevX || y != prevY) {
            var slope = (y - prevY) / (x - prevX).toDouble
            if (slope.abs > 1) {
              for (py <- prevY to y by (y - prevY).signum) {
                circ(brush, ((py - prevY) / slope + prevX).round.toInt, py, size)
              }
            }
            else /* if (slope.abs < 1) */ {
              for (px <- prevX to x by (x - prevX).signum) {
                circ(brush, px, ((px - prevX) * slope + prevY).round.toInt, size)
              }
            }
          }
      }
      prevX = action.x
      prevY = action.y
    }
  }
  val arraydx = Array(-1, -1, -1, 0, 0, 1, 1, 1)
  val arraydy = Array(-1, 0, 1, -1, 1, -1, 0, 1)
  def step() = {
    var ix = 0
    while (ix < state.count) {
      val p = state(ix)
      val pt = p.v
      var falling = false
      if ((pt.gravity > 0 && p.y < h - 1) || (pt.gravity < 0 && p.y > 0)) {
        if (rnd.nextDouble() < pt.gravity.abs) {
          val pos = 
            if (pt.spread > 0 && rnd.nextDouble() < pt.spread)
              (p.x + rnd.nextInt(3) - 1) max 0 min (w - 1)
            else
              p.x
          val it = state(pos, p.y + pt.gravity.signum)
          if (it == null) {
            state.move(p, pos, p.y + pt.gravity.signum)
            falling = true
          }
          else if (it.v.density < pt.density || pt.gravity.signum * it.v.gravity.signum == -1) {
            state.swap(p, it)
            falling = true
          }
        }
      }
      if (!falling && pt.slide > 0) {
        if (rnd.nextDouble() < pt.slide) {
          val pos = (p.x + rnd.nextInt(2) * 2 - 1) max 0 min (w - 1)
          val it = state(pos, p.y)
          if (it == null) {
            state.move(p, pos, p.y)
          }
          else if (it.v.slide > 0) {
            state.swap(p, it)
          }
        }
      }
      var i = 0
      var interacted = false
      val it = rnd.nextInt(arraydx.length - i) + i
      val dx = arraydx(it)
      val dy = arraydy(it)
      if (p.x + dx >= 0 && p.y + dy >= 0 && p.x + dx < w && p.y + dy < h) {
        val o = {
          val q = state(p.x + dx, p.y + dy)
          if (q == null) mod.empty
          else q.v
        }
        val interaction = mod.interact(pt, o)
        if (!interaction.isEmpty) {
          var r = rnd.nextDouble()
          var i = 0
          while (i < interaction.length && r > interaction(i)._1) {
            r -= interaction(i)._1
            i += 1
          }
          if (i < interaction.length) {
            interacted = true
            place(interaction(i)._2, p.x, p.y)
            if (o != interaction(i)._3) {
              place(interaction(i)._3, p.x + dx, p.y + dy)
            }
          }
        }
      }
      if (!interacted) {
        val self = mod.self(pt)
        if (!self.isEmpty) {
            var r = rnd.nextDouble()
            var i = 0
            while (i < self.length && r > self(i)._1) {
              r -= self(i)._1
              i += 1
            }
            if (i < self.length) {
              interacted = true
              place(self(i)._2, p.x, p.y)
            }
        }
      }
      ix += 1
    }
  }
  def render() = {
    val res = new BufferedImage(3 * w, 3 * h, BufferedImage.TYPE_INT_ARGB)
    val gfx = res.getGraphics
    gfx.setColor(back)
    gfx.fillRect(0, 0, 3 * w, 3 * h)
    var ix = 0
    while (ix < state.count) {
      val p = state(ix)
      gfx.setColor(makeColor(p.v.color)) // TODO: make faster by caching Color objects
      gfx.fillRect(3 * p.x, 3 * p.y, 3, 3)
      ix += 1
    }
    img = res
    repaint()
  }
  
  override def paintComponent(gfx: Graphics) = {
    val minX = getWidth() / 2 - w * 3 / 2
    val minY = getHeight() / 2 - h * 3 / 2
    gfx.drawImage(img, minX, minY, this)
  }
  
  addMouseListener(new MouseListener() {
    def mouseClicked(ev: MouseEvent) = {}
    def mouseEntered(ev: MouseEvent) = {}
    def mouseExited(ev: MouseEvent) = {}
    def mouseReleased(ev: MouseEvent) = {}
    def mousePressed(ev: MouseEvent) = {
      val minX = getWidth() / 2 - w * 3 / 2
      val minY = getHeight() / 2 - h * 3 / 2
      if (ev.getButton == MouseEvent.BUTTON1) {
        paintQueue.add(InitAction((ev.getX - minX) / 3, (ev.getY - minY) / 3, brush, brushSize))
      }
      else if (ev.getButton == MouseEvent.BUTTON3) {
        paintQueue.add(DragAction((ev.getX - minX) / 3, (ev.getY - minY) / 3, brush, brushSize))
      }
    }
  })
  addMouseMotionListener(new MouseMotionListener() {
    def mouseMoved(ev: MouseEvent) = {}
    def mouseDragged(ev: MouseEvent) = {
      val minX = getWidth() / 2 - w * 3 / 2
      val minY = getHeight() / 2 - h * 3 / 2
      paintQueue.add(DragAction((ev.getX - minX) / 3, (ev.getY - minY) / 3, brush, brushSize))
    }
  })
  
}