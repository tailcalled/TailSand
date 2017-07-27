package tailsand.mods

import parsers._

object ModParser {
  
  sealed trait Token
  case object KElement extends Token
  case object KTag extends Token
  case object KSep extends Token
  case object KListSep extends Token
  case object KArity extends Token
  case object KRelation extends Token
  case object KGravity extends Token
  case object KSpread extends Token
  case object KDensity extends Token
  case object KSlide extends Token
  case object KDescription extends Token
  case object KReaction extends Token
  case object KSelf extends Token
  case object KTitle extends Token
  case object KName extends Token
  case object KWhere extends Token
  case object KBegin extends Token
  case object KEnd extends Token
  case object KTo extends Token
  case class LColor(r: Int, g: Int, b: Int) extends Token
  case class LString(value: String) extends Token
  case class LNumber(value: Double) extends Token
  case class Name(name: String) extends Token
  case class Free(name: String) extends Token
  
  lazy val kElement = for (meh @ KElement <- token) yield KElement
  lazy val kTag = for (buggyness @ KTag <- token) yield KTag
  lazy val kSep = for (meh @ KSep <- token) yield KSep
  lazy val kListSep = for (meh @ KListSep <- token) yield KListSep
  lazy val kArity = for (meh @ KArity <- token) yield KArity
  lazy val kRelation = for (meh @ KRelation <- token) yield KRelation
  lazy val kGravity = for (scalacompilerbug @ KGravity <- token) yield KGravity
  lazy val kSpread = for (meh @ KSpread <- token) yield KSpread
  lazy val kDensity = for (bluh @ KDensity <- token) yield KDensity
  lazy val kSlide = for (bugs @ KSlide <- token) yield KSlide
  lazy val kDescription = for (annoying @ KDescription <- token) yield KDescription
  lazy val kReaction = for (meh @ KReaction <- token) yield KReaction
  lazy val kSelf = for (meh @ KSelf <- token) yield KSelf
  lazy val kTitle = for (meh @ KTitle <- token) yield KTitle
  lazy val kName = for (meh @ KName <- token) yield KName
  lazy val kWhere = for (gahh @ KWhere <- token) yield KWhere
  lazy val kBegin = for (meh @ KBegin <- token) yield KBegin
  lazy val kEnd = for (meh @ KEnd <- token) yield KEnd
  lazy val kTo = for (meh @ KTo <- token) yield KTo
  lazy val name = for (meh @ Name(name) <- token) yield Name(name)
  lazy val lColor = for (meh @ LColor(r, g, b) <- token) yield LColor(r, g, b)
  lazy val lString = for (meh @ LString(value) <- token) yield LString(value)
  lazy val lNumber = for (meh @ LNumber(value) <- token) yield LNumber(value)
  lazy val lInt = for (meh @ LNumber(value) <- token; if value.round == value) yield value.round.toInt
  lazy val wildcard = for (meh @ Free(name) <- token) yield Free(name)
  
  lazy val namePart: Parser[String] = char.filter(_.isUnicodeIdentifierPart).star.map(_.mkString)
  lazy val _name = {
    for {
      x <- char
      if x.isUnicodeIdentifierStart
      y <- namePart
    } yield x + y
  }
  lazy val color = for {
    col <- lit("#") *> namePart
    if col.length == 6 && col.forall { ch => ch >= '0' && ch <= '9' || ch >= 'A' && ch <= 'F' }
  } yield {
    val it = Integer.parseInt(col, 16)
    LColor((it >> 16) & 0xFF, (it >>  8) & 0xFF, (it >>  0) & 0xFF)
  }
  lazy val _string = {
    val stringChar = lit("\\") *> char ++ char.filter(ch => ch != '\"' && ch != '\\')
    lit("\"") *> stringChar.star *< lit("\"") map { case str => LString(str.mkString) }
  }
  lazy val _number = {
    val digits = char.filter(ch => ch >= '0' && ch <= '9').plus
    val num = lit("-").opt ** digits ** (lit(".") *> digits.map ('.' +: _)).opt
    num.map {
      case sign ** int ** frac =>
        (sign.getOrElse("") + int.mkString + frac.map(_.mkString).getOrElse("")).toDouble
    }.map(LNumber(_))
  }
  lazy val _wildcard = lit("'") *> _name map { case str => Free(str) }
  lazy val _token =
    _name.map {
      case "element" => KElement
      case "tag" => KTag
      case "relation" => KRelation
      case "where" => KWhere
      case "end" => KEnd
      case "gravity" => KGravity
      case "spread" => KSpread
      case "density" => KDensity
      case "slide" => KSlide
      case "description" => KDescription
      case "reaction" => KReaction
      case "self" => KSelf
      case "name" => KName
      case "title" => KTitle
      case name => Name(name)
    } ++
    color ++
    lit(";").replace(KSep) ++
    lit(",").replace(KListSep) ++
    lit("/").replace(KArity) ++
    lit(":").replace(KBegin) ++
    lit("=>").replace(KTo) ++
    _string ++
    _number ++
    _wildcard
  lazy val space = lit(" ") ++ lit("\n") ++ lit("\r") ++ lit("\t")
  lazy val lineComment = lit("--") ** char.filter(ch => ch != '\n' && ch != '\r').star ** lit("\r") replace ()
  lazy val blockComment = lit("{-") ** (char.filter(ch => ch != '-') ++ lit("-") ** char.filter(ch => ch != '}')).star ** lit("-}") replace ()
  lazy val sur = (space ++ lineComment ++ blockComment).star
  lazy val token = sur *> _token *< sur
  
  lazy val mod = block map { case stmts => Mod(stmts) }
  lazy val block: Parser[ModStatement] = (statement *< kSep).star map { case body => Block(body) }
  lazy val statement =
    elemStatement ++ tagStatement ++ relationDecStatement ++ relationStatement ++ whereBlock ++ gravityProp ++ spreadProp ++
    densityProp ++ slideProp ++ descriptionProp ++ reactionStatement ++ selfStatement ++ nameProp ++ titleStatement
  lazy val elemStatement = kElement *> element ** lColor map { case elem ** LColor(r, g, b) => ElementDec(elem, (r, g, b)) }
  lazy val tagStatement = kTag *> elementRef ** lString map { case elem ** LString(tag) => TagDec(elem, tag) }
  lazy val relationDecStatement = kRelation *> name *< kArity ** lInt map { case Name(name) ** arity => RelDec(name, arity) }
  lazy val relationStatement = name ** elementRef.star map { case Name(rel) ** elems => RelStatement(rel, elems) }
  lazy val whereBlock = kWhere *> proposition *< kBegin ** block *< kEnd map {
    case prop ** body => prop.foldRight(body)((prop, inner) => Where(prop, inner))
  }
  lazy val proposition = propPart ** (kListSep *> propPart).star map { case first ** rest => first +: rest }
  lazy val propPart = name ** elementRef.star map { case Name(rel) ** elems => Proposition(rel, elems) }
  lazy val element = name map { case Name(name) => Element(name) }
  lazy val elementRef = element ++ wildcard.map { case Free(name) => Wildcard(name) }
  lazy val gravityProp = kGravity *> elementRef ** lNumber map { case elem ** grav => GravityProp(elem, grav.value) }
  lazy val spreadProp = kSpread *> elementRef ** lNumber map { case elem ** spread => SpreadProp(elem, spread.value) }
  lazy val densityProp = kDensity *> elementRef ** lNumber map { case elem ** density => DensityProp(elem, density.value) }
  lazy val slideProp = kSlide *> elementRef ** lNumber map { case elem ** slide => SlideProp(elem, slide.value) }
  lazy val descriptionProp = kDescription *> elementRef ** lString map { case elem ** description => DescriptionProp(elem, description.value) }
  lazy val reactionStatement = kReaction *> lNumber ** elementRef ** elementRef *< kTo ** elementRef ** elementRef map {
    case rate ** elem1 ** elem2 ** res1 ** res2 => ReactionStatement(rate.value, elem1, elem2, res1, res2)
  }
  lazy val selfStatement = kSelf *> lNumber ** elementRef *< kTo ** elementRef map {
    case rate ** elem ** res => SelfStatement(rate.value, elem, res)
  }
  lazy val nameProp = kName *> elementRef ** lString map { case elem ** slide => NameProp(elem, slide.value) }
  lazy val titleStatement = kTitle *> lString map { case title => TitleStatement(title.value) }
  
}