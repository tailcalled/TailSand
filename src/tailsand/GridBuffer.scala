package tailsand

class GridBuffer[T](width: Int, height: Int) {
  
  class Value(
      private[GridBuffer] var ix: Int,
      private[GridBuffer] var _x: Int,
      private[GridBuffer] var _y: Int,
      private[GridBuffer] var active: Boolean,
      val v: T) {
    @inline def x = _x
    @inline def y = _y
  }
  
  private var _count = 0
  private val elems = Array.fill[Value](width * height)(null)
  private val grid = Array.fill[Value](width, height)(null)
  
  def count = _count
  def delete(v: Value) = {
    if (!v.active) throw new Exception()
    v.active = false
    grid(v.x)(v.y) = null
    val last = elems(_count - 1)
    elems(v.ix) = last
    last.ix = v.ix
    _count -= 1
  }
  def move(v: Value, x: Int, y: Int) = {
    if (!v.active) throw new Exception()
    if (grid(x)(y) != null) throw new Exception
    grid(v.x)(v.y) = null
    grid(x)(y) = v
    v._x = x
    v._y = y
  }
  def swap(v1: Value, v2: Value) = {
    if (!v1.active) throw new Exception()
    if (!v2.active) throw new Exception()
    val tx = v1.x
    val ty = v1.y
    v1._x = v2.x
    v1._y = v2.y
    v2._x = tx
    v2._y = ty
    grid(v1.x)(v1.y) = v1
    grid(v2.x)(v2.y) = v2
  }
  def create(t: T, x: Int, y: Int) = {
    if (grid(x)(y) != null) throw new Exception
    val it = new Value(_count, x, y, true, t)
    grid(x)(y) = it
    elems(_count) = it
    _count += 1
    it
  }
  def apply(x: Int, y: Int) = grid(x)(y)
  def apply(ix: Int) = {
    if (ix >= _count) throw new Exception
    elems(ix)
  }
  
}