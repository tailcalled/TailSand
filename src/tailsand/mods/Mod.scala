package tailsand.mods

import java.io.File
import tailsand.SandMod
import tailsand.ParticleType

case class Mod(mod: ModStatement)

sealed trait ModStatement
case class ElementDec(elem: Element, color: (Int, Int, Int)) extends ModStatement
case class TagDec(elem: ElementRef, tag: String) extends ModStatement
case class RelDec(id: String, arity: Int) extends ModStatement
case class RelStatement(rel: String, elems: Vector[ElementRef]) extends ModStatement
case class GravityProp(elem: ElementRef, grav: Double) extends ModStatement
case class SpreadProp(elem: ElementRef, spread: Double) extends ModStatement
case class DensityProp(elem: ElementRef, density: Double) extends ModStatement
case class SlideProp(elem: ElementRef, slide: Double) extends ModStatement
case class DescriptionProp(elem: ElementRef, description: String) extends ModStatement
case class ReactionStatement(rate: Double, elem1: ElementRef, elem2: ElementRef, res1: ElementRef, res2: ElementRef) extends ModStatement
case class SelfStatement(rate: Double, elem: ElementRef, res: ElementRef) extends ModStatement
case class NameProp(elem: ElementRef, name: String) extends ModStatement
case class TitleStatement(title: String) extends ModStatement
case class Where(prop: Proposition, body: ModStatement) extends ModStatement
case class Block(body: Vector[ModStatement]) extends ModStatement

sealed trait ElementRef
case class Wildcard(id: String) extends ElementRef
case class Element(id: String) extends ElementRef

case class Proposition(rel: String, elems: Vector[ElementRef])

object Mod {
  
  def load(file: File): SandMod = {
    val modData = parsers.parse(ModParser.mod, file)
    println(modData)
    val mod = new LoadedSandMod()
    def deref(el: ElementRef) = el match {
      case Wildcard(wildcard) => throw new Exception(s"unbound wildcard $wildcard")
      case Element(name) => mod.particles(name)
    }
    def add(statement: ModStatement): Unit = {
      statement match {
        case Block(body) =>
          for (stmt <- body) {
            add(stmt)
          }
        case ElementDec(Element(id), color) =>
          if (mod.particles.contains(id)) throw new Exception(s"id $id already in use")
          val particle = new mod.Particle(id, color)
          mod.particles += id -> particle
        case TagDec(elem, tag) =>
          mod.tags += tag -> (mod.tags(tag) :+ deref(elem))
        case RelDec(id, arity) =>
          if (mod.relations.contains(id)) throw new Exception(s"relation id $id already in use")
          mod.relations += id -> mod.Relation(arity, Vector(), Vector())
        case RelStatement(name, elems) =>
          val rel = mod.relations(name)
          if (rel.arity != elems.length) throw new Exception("incorrect arity")
          val parts = elems.map(deref _)
          mod.relations += name -> rel.copy(matching = rel.matching :+ parts)
          for ((pattern, stmt) <- rel.listening) {
            tryApply(parts, pattern, stmt)
          }
        case GravityProp(name, grav) =>
          deref(name).gravity = grav
        case SpreadProp(name, spread) =>
          deref(name).spread = spread
        case DensityProp(name, density) =>
          deref(name).density = density
        case SlideProp(name, slide) =>
          deref(name).slide = slide
        case DescriptionProp(name, desc) =>
          deref(name).description += desc
        case ReactionStatement(rate, elem1, elem2, res1, res2) =>
          val e1 = deref(elem1); val e2 = deref(elem2)
          mod.interactions += (e1, e2) -> (mod.interactions((e1, e2)) :+ (rate, deref(res1), deref(res2)))
        case SelfStatement(rate, elem, res) =>
          val e = deref(elem)
          mod.selfs += e -> (mod.selfs(e) :+ (rate, deref(res)))
        case TitleStatement(title) =>
          mod.name = title
        case NameProp(elem, name) =>
          deref(elem).name = name
        case Where(prop, body) =>
          val rel = mod.relations(prop.rel)
          if (rel.arity != prop.elems.size) throw new Exception("incorrect arity")
          mod.relations += prop.rel -> rel.copy(listening = rel.listening :+ (prop.elems, body))
          for (parts <- rel.matching) {
            tryApply(parts, prop.elems, body)
          }
      }
    }
    def tryApply(parts: Vector[mod.Particle], pattern: Vector[ElementRef], statement: ModStatement): Unit = {
      var assignment = Map[String, mod.Particle]()
      for ((pt, ref) <- parts zip pattern) {
        ref match {
          case Wildcard(name) =>
            if (assignment.get(name).forall(_ == pt)) {
              assignment += name -> pt
            }
            else return ()
          case Element(name) =>
            if (mod.particles(name) != pt) return ()
        }
      }
      add(subst(assignment, statement))
    }
    def substRef(map: Map[String, mod.Particle], ref: ElementRef): ElementRef = ref match {
      case Wildcard(name) if map.contains(name) => Element(map(name).code)
      case x => x
    }
    def subst(map: Map[String, mod.Particle], statement: ModStatement): ModStatement = {
      statement match {
        case Block(body) => Block(body.map(subst(map, _)))
        case TagDec(ref, tag) => TagDec(substRef(map, ref), tag)
        case RelDec(name, arity) => RelDec(name, arity)
        case ElementDec(name, color) => ElementDec(name, color)
        case RelStatement(name, elems) =>
          val substElems = elems.map(substRef(map, _))
          RelStatement(name, substElems)
        case GravityProp(name, grav) => GravityProp(substRef(map, name), grav)
        case SpreadProp(name, spread) => SpreadProp(substRef(map, name), spread)
        case DensityProp(name, density) => DensityProp(substRef(map, name), density)
        case SlideProp(name, slide) => SlideProp(substRef(map, name), slide)
        case DescriptionProp(name, desc) => DescriptionProp(substRef(map, name), desc)
        case ReactionStatement(rate, elem1, elem2, res1, res2) =>
          ReactionStatement(rate, substRef(map, elem1), substRef(map, elem2), substRef(map, res1), substRef(map, res2))
        case SelfStatement(rate, elem, res) => SelfStatement(rate, substRef(map, elem), substRef(map, res))
        case TitleStatement(title) => TitleStatement(title)
        case NameProp(elem, name) => NameProp(substRef(map, elem), name)
        case Where(prop, body) =>
          val substProp = prop.elems.map(substRef(map, _))
          val substBody = subst(map, body)
          Where(prop.copy(elems = substProp), substBody)
      }
    }
    add(modData.mod)
    println(mod.relations)
    mod
  }
  
}

class LoadedSandMod extends SandMod {
  
  case class Relation(arity: Int, matching: Vector[Vector[Particle]], listening: Vector[(Vector[ElementRef], ModStatement)])

  class Particle(var name: String, val color: (Int, Int, Int)) extends ParticleType {
    val code = name
    var description = ""
    var gravity = 0.0
    var slide = 0.0
    var spread = 0.0
    var density = 0.0
    override def toString = code
  }
  val empty = new Particle("Empty", (0, 0, 0))
  var particles = Map("Empty" -> empty)
  var tags = Map[String, Vector[Particle]]().withDefaultValue(Vector())
  var relations = Map[String, Relation]()
  var name = "Unnamed Sand Mod"
  var interactions = Map[(Particle, Particle), Vector[(Double, Particle, Particle)]]().withDefaultValue(Vector())
  var selfs = Map[Particle, Vector[(Double, Particle)]]().withDefaultValue(Vector())
  def interact(p1: Particle, p2: Particle) = interactions((p1, p2))
  def self(p: Particle) = selfs(p)
  
}