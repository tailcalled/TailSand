package tailsand

trait ParticleType {
  def color: (Int, Int, Int)
  def name: String
  def description: String
  def gravity: Double
  def slide: Double
  def spread: Double
  def density: Double
}
trait SandMod {
  
  type Particle <: ParticleType
  
  def empty: Particle
  def tags: Map[String, Vector[Particle]]
  def name: String
  def interact(p1: Particle, p2: Particle): Vector[(Double, Particle, Particle)]
  def self(p1: Particle): Vector[(Double, Particle)]
  
}
object DefaultMod extends SandMod {
  
  sealed trait Particle extends ParticleType {
    val name = toString
  }
  case object Empty extends Particle {
    val color = (0, 0, 0)
    val description = "Void."
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 0.0
  }
  case object Sand extends Particle {
    val color = (255, 255, 127)
    val description = "It falls. That's the point. This is a falling sand game."
    val gravity = 1.0
    val slide = 0.0
    val spread = 0.1
    val density = 0.6
  }
  case object Water extends Particle {
    val color = (0, 0, 255)
    val description = "It's wet."
    val gravity = 1.0
    val slide = 1.0
    val spread = 0.3
    val density = 0.3
  }
  case object Spring extends Particle {
    val color = (127, 127, 255)
    val description = "Creates water out of nothing. Magic."
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 1.0
  }
  case object Plant extends Particle {
    val color = (0, 127, 0)
    val description = "It noms water to grow BIG."
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 1.0
  }
  case object Wall extends Particle {
    val color = (127, 127, 127)
    val description = "It does nothing. It just sits there. So lazy."
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 1.0
  }
  case object Fire1 extends Particle {
    val color = (255, 223, 0)
    override val name = "Fire"
    val description = "Burns everything."
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 0.5
  }
  case object Fire2 extends Particle {
    val color = (255, 127, 0)
    val description = ""
    val gravity = -0.5
    val slide = 0.0
    val spread = 0.0
    val density = 0.5
  }
  case object Fire3 extends Particle {
    val color = (255, 0, 0)
    val description = ""
    val gravity = -1.0
    val slide = 0.0
    val spread = 0.0
    val density = 0.5
  }
  case object Steam extends Particle {
    val color = (200, 200, 255)
    val description = "Hot water."
    val gravity = -1.0
    val slide = 0.0
    val spread = 0.0
    val density = 0.7
  }
  trait Clear
  case object Clear1 extends Particle with Clear {
    val color = (255, 0, 0)
    override val name = "Clear"
    val description = "Clears the everything."
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 1.0
  }
  case object Clear2 extends Particle with Clear {
    val color = (0, 255, 0)
    val description = ""
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 1.0
  }
  case object Clear3 extends Particle with Clear {
    val color = (0, 0, 255)
    val description = ""
    val gravity = 0.0
    val slide = 0.0
    val spread = 0.0
    val density = 1.0
  }
  
  val tags = Map("(all)" -> Vector(Empty, Sand, Water, Wall, Plant, Fire1, Spring, Clear1), "falling" -> Vector(Sand, Water), "interacting" -> Vector(Water, Plant, Fire1, Spring))
  val empty = Empty
  val name = "DefaultMod"
  def interact(p1: Particle, p2: Particle) = (p1, p2) match {
    case (Water, Plant) => Vector((1.0, Plant, Plant))
    case (Plant, Fire1) => Vector((1.0, Fire1, Fire1))
    case (Plant, Fire2) => Vector((1.0, Fire1, Fire2))
    case (Plant, Fire3) => Vector((1.0, Fire1, Fire3))
    case (Spring, Empty) => Vector((1.0, Spring, Water))
    case (Water, Fire1) | (Water, Fire2) | (Water, Fire3) => Vector((0.1, Steam, Fire3), (0.4, Steam, Empty), (0.5, Water, Empty))
    case (Clear1, x) if !(x.isInstanceOf[Clear]) => Vector((1.0, Clear1, Clear1))
    case (Clear2, x) if x != Empty && ! (x.isInstanceOf[Clear]) => Vector((1.0, Clear2, Clear1))
    case (Clear3, Clear1) => Vector((1.0, Clear2, Clear2))
    case _ => Vector()
  }
  def self(p1: Particle) = p1 match {
    case Fire1 => Vector((0.20, Fire2))
    case Fire2 => Vector((0.20, Fire3))
    case Fire3 => Vector((0.20, Empty))
    case Clear1 => Vector((0.20, Clear2))
    case Clear2 => Vector((0.03, Clear3))
    case Clear3 => Vector((0.03, Empty))
    case Steam => Vector((0.05, Water))
    case _ => Vector()
  }
  
}