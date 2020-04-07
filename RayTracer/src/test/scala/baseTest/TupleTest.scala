package baseTest

import base.Tuple
import org.scalatest._

class TupleTest extends FlatSpec with Matchers {

  "A point" should "have a w value of 1.0" in {
    val point = new Tuple(1,2,3,1)
    point.isPoint() should be (true)
    point.isVector() should be (false)
    point.isValid() should be (true)
  }

  it should "add with a vector to make a point" in {
    val point = new Tuple(3,-2, 5, 1)
    val vec = new Tuple(-2, 3, 1, 0)
    val result = point + vec
    result.isValid() should be (true)
    result.isVector() should be (false)
    result.isPoint() should be (true)
    result._x should be (1)
    result._y should be (1)
    result._z should be (6)
  }

  it should "subtract with a vector to make a point" in {
    val point = new Tuple(3,2, 1, 1)
    val vec = new Tuple(5, 6, 7, 0)
    val result = point - vec
    result.isValid() should be (true)
    result.isPoint() should be (true)
    result.isVector() should be (false)
    result._x should be (-2)
    result._y should be (-4)
    result._z should be (-6)
  }

  it should "subtract with a point to make a vector" in {
    val a = new Tuple(3,2, 1, 1)
    val b = new Tuple(5, 6, -6, 1)
    val result = a - b
    result.isVector() should be (true)
    result._x should be (-2)
  }

  it should "negate to make nonsense" in {
    val a = new Tuple(1,1,1,1)
    val r = a negate()
    r.isValid() should be (false)
    r._x should be (-1)
  }

  it should "divide by a scalar to make a vector" in {
    val a = new Tuple(1, 2, 3, 1)
    val r1 = a / 2
    val r2 = a / 2.2
    val z2: Double = 3 / 2.2
    r1.isVector should be (true)
    r2.isVector() should be (true)
    r1._z should be (1.5)
    r2._z should be (z2)
  }

  it should "divide by a point to make a vector" in {
    val a = new Tuple(1, 2, 6, 1)
    val b = new Tuple(1,1,2,1)
    val r = a / b
    r.isPoint() should be (true)
    r._z should be (3)
  }

  "A vector" should "have a w value of 0.0" in {
    val vec = new Tuple(3,2,1,0)
    vec.isPoint() should be (false)
    vec.isVector() should be (true)
    vec.isValid() should be (true)
  }

  it should "add with another vector to make a new vector" in {
    val a = new Tuple(3, -2, 5, 0)
    val b = new Tuple(-2, 3, 1, 0)
    val result = a + b
    result.isVector() should be (true)
    result.isPoint() should be (false)
    result.isValid() should be (true)
    result._x should be (1)
    result._y should be (1)
    result._z should be (6)
  }

  it should "negate to a new negative vector" in {
    val t = new Tuple(1,2,3,0)
    val r = t negate()
    r.isVector() should be (true)
    r._x should be (-1)
    r._y should be (-2)
    r._z should be (-3)
  }

  it should "multiply by any scalar to form a vector" in {
    val a = new Tuple(1,2,3,0)
    val r1 = a * 2
    val r2 = a * 3.2
    r1.isVector() should be (true)
    r2.isVector() should be (true)
    r1._y should be (4)
    r2._y should be (6.4)
  }

  it should "have a valid magnitude" in {
    //first three should equal 1
    val a = new Tuple(1, 0, 0, 1)
    val b = new Tuple(0, 1, 0, 1)
    val c = new Tuple(0, 0, 1, 1)
    //and the next two should equal the square root of 14
    val d = new Tuple(1,2,3,1)
    val e = new Tuple(-1,-2,-3,1)
    //results
    val r1 = 1
    val r2 = math.pow(14, 0.5)
    a.magnitude() should be (r1)
    b.magnitude() should be (r1)
    c magnitude() should be (r1)
    d magnitude() should be (r2)
    e magnitude() should be (r2)
  }

  it should "allow for unit vectors" in {
    val a = new Tuple(1, 0, 0,0)
    val b = new Tuple(1, 0, 0, 1)
    val c = new Tuple(3,3,3,0)
    a isUnitVector() should be (true)
    b isUnitVector() should be (false)
    c isUnitVector() should be (false)
  }

  it should "be able to be normalized" in {
    val a = new Tuple(2, 4, 6, 0)
    val b = new Tuple(9, 3, 5, 0)
    val c = new Tuple(0, 0.5, 0, 0)
    val d = new Tuple(-3, 5, 2, 0)
    a normalize() magnitude() should be (1)
    b normalize() magnitude() should be (1)
    c normalize() magnitude() should be (1)
    d normalize() magnitude() should be (1)
  }

  it should "be able to have a dot product with another vector" in {
    val a = new Tuple(1, 2,3, 0)
    val b = new Tuple(2, 3, 4, 0)
    a dotProduct(b) should be (20)
  }

  it should "allow for a cross product" in {
    val a = new Tuple(1, 2,3,0)
    val b = new Tuple(2, 3, 4, 0)
    val resultA = a crossProduct(b)
    val resultB = b crossProduct(a)
    resultA._x should be (-1)
    resultA._y should be (2)
    resultA._z should be (-1)
    resultB._x should be (1)
    resultB._y should be (-2)
    resultB._z should be (1)
  }

  "Nonsense" should "have any w value" in {
    val t = new Tuple(1,1,1,4)
    t.isPoint() should be (false)
    t.isValid() should be (false)
    t.isVector() should be (false)
  }

}
