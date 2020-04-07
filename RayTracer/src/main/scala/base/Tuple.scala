package base

/**
  * base.Tuple class for ray tracer, will have point or vector subclasses.
  */
class Tuple(val _x: Double, val _y: Double, val _z: Double, val _w: Integer) {

  /**
    * Overriding standard addition operator.
    * @param other what to add
    * @return a new tuple with the result
    */
  def +(other: Any): Tuple = other match {
    //If we have a number
    case n : Number => {
      val value: Double = n.doubleValue()
      val x = (_x + value) doubleValue()
      val y = (_y + value) doubleValue()
      val z = (_z + value) doubleValue()
      val w = (_w + value) intValue()
      new Tuple(x, y, z, w)
    }

    //if we have a tuple
    case t: Tuple => {
      val x = _x + t._x
      val y = _y + t._y
      val z = _z + t._z
      val w = _w + t._w
      new Tuple(x, y, z, w)
    }

    case _ => null
  }

  /**
    * Overriding standard subtraction operator.
    * @param other what to subtract from this
    * @return a new tuple with the result
    */
  def -(other: Any): Tuple = other match {
    //if we have a scalar value
    case n: Number => {
      val value: Double = n.doubleValue()
      val x = (_x - value) doubleValue()
      val y = (_y - value) doubleValue()
      val z = (_z - value) doubleValue()
      val w = (_w - value) intValue()
      new Tuple(x, y, z, w)
    }

    //if we have a tuple
    case t: Tuple => {
      val x = _x - t._x
      val y = _y - t._y
      val z = _z - t._z
      val w = _w - t._w
      new Tuple(x, y, z, w)
    }

    //otherwise
    case _ => null
  }

  /**
    * Overriding standard multiplication operator.
    * @param other what to multiply this by
    * @return a new tuple with the result
    */
  def *(other: Any): Tuple = other match {
    //if we have a scalar
    case n: Number => {
      val value: Double = n.doubleValue()
      val x = (_x * value) doubleValue()
      val y = (_y * value) doubleValue()
      val z = (_z * value) doubleValue()
      val w = (_w * value) intValue()
      new Tuple(x, y, z, w)
    }

    //if we have a tuple
    case t: Tuple => {
      val x = _x * t._x
      val y = _y * t._y
      val z = _z * t._z
      val w = _w * t._w
      new Tuple(x, y, z, w)
    }

    //otherwise, failure
    case _ => null
  }

  /**
    * Overriding standard division operator.
    * @param other what to divide this by
    * @return a new tuple with the result
    */
  def /(other: Any): Tuple = other match {
    //if we have a scalar
    case n: Number => {
      val value: Double = n.doubleValue()
      val x = (_x / value) doubleValue()
      val y = (_y / value) doubleValue()
      val z = (_z / value) doubleValue()
      val w = (_w / value) intValue()
      new Tuple(x, y, z, w)
    }

    //if we have a tuple
    case t: Tuple => {
      val x = _x / t._x
      val y = _y / t._y
      val z = _z / t._z
      val w = _w / t._w
      new Tuple(x, y, z, w)
    }

    //otherwise, failure
    case _ => null
  }

  /**
    * Overriding the standard modulo operator.
    * @param other what to modulo this by
    * @return a new tuple with the result
    */
  def %(other: Any): Tuple = other match {
    //if we have a scalar
    case n: Number => {
      val value: Double = n.doubleValue()
      val x = (_x % value) doubleValue()
      val y = (_y % value) doubleValue()
      val z = (_z % value) doubleValue()
      val w = (_w % value) intValue()
      new Tuple(x, y, z, w)
    }

    //if we have a tuple
    case t: Tuple => {
      val x = _x % t._x
      val y = _y % t._y
      val z = _z % t._z
      val w = _w % t._w
      new Tuple(x, y, z, w)
    }

    //otherwise, failure
    case _ => null
  }

  /**
    * Return a negative version of this
    * @return new tuple with result
    */
  def negate(): Tuple = {
    val x = -1 * _x
    val y = -1 * _y
    val z = -1 * _z
    val w = -1 * _w
    new Tuple(x, y, z, w)
  }

  /**
    * Pythagoras' theorem for magnitude
    * @return double value of result
    */
  def magnitude(): Double = {
    val a = math.pow(_x, 2)
    val b = math.pow(_y, 2)
    val c = math.pow(_z, 2)
    val total = a + b + c
    math.pow(total, 0.5)
  }

  /**
    * Normalize a vector into a unit vector
    * @return new tuple unit vector
    */
  def normalize(): Tuple = {
    val mag = magnitude()
    val x = _x / mag
    val y = _y / mag
    val z = _z / mag
    new Tuple(x, y, z, _w)
  }

  /**
    * Standard dot product
    * @param other other tuple
    * @return double result value
    */
  def dotProduct(other: Tuple): Double = {
    val a = _x * other._x
    val b = _y * other._y
    val c = _z * other._z
    val d = _w * other._w
    a + b + c + d
  }

  /**
    * Cross product of two vectors
    * @param other the second vector
    * @return a new vector with result
    */
  def crossProduct(other: Tuple): Tuple = {
    val x = (_y * other._z) - (_z * other._y)
    val y = (_z * other._x) - (_x * other._z)
    val z = (_x * other._y) - (_y * other._x)
    val w = 0
    new Tuple(x, y, z, w)
  }

  /**
    * Is this a point, if _w is 1
    * @return boolean result
    */
  def isPoint(): Boolean = _w.equals(1)

  /**
    * Is this a vector, if _w is 0
    * @return boolean result
    */
  def isVector(): Boolean = _w.equals(0)

  /**
    * Is this a vector or a point
    * @return boolean result
    */
  def isValid(): Boolean = isPoint() || isVector()

  def isUnitVector(): Boolean = (magnitude() equals 1.0) && isVector()

  /**
    * If its a color, it can be sent to RGB format.
    * @return the new color
    */
  def toRGB(): Tuple = {
    val r = _x * 255
    val g = _y * 255
    val b = _z * 255
    new Tuple(r, g, b, -1)
  }


}