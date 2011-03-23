// Minitest (c) 2011 Jamie Webb
// MIT License

package cc.jwebb.minitest

import org.junit.Assert._

/** Used as an implicit parameter to control the margin for error allowed in
 * floating point equality tests. */
class Epsilon(val v : Double) {
	/** Default value. */
	def this() = this(0.000000001)
}

/** Test assertions on Objects. */
class TestObjectOps[T](me : T) {
	private def show(x : Any) : String = x match {
		case null =>
			"(null)"
		case a : Array[_] =>
			a.view.map(show).mkString("Array(", ", ", ")")
		case _ =>
			x.toString
	}

	/** Equality test. */
	def ==?(that : AnyRef) = assertEquals(that, me)

	/** Inequality test. */
	def !=?(that : AnyRef) =
		if (me == that)
			fail("Expected " + show(me) + " != " + show(that))

	/** Non-null test. */
	def ? = assertNotNull(me)

	/** Regex matching test. */
	def =~?(that : String) = {
		assertNotNull(me)
		assertTrue("Expected '" + show(me) + "' to match '" + that + "'",
				show(me).matches(that))
	}
}

/** Test assertions on Longs. */
class TestLongOps(me : Long) {
	def ==?(that : Long) = assertTrue("Expected " + me + " == " + that, me == that)
	def !=?(that : Long) = assertTrue("Expected " + me + " != " + that, me != that)
	def <<?(that : Long) = assertTrue("Expected " + me + " < " + that, me < that)
	def <=?(that : Long) = assertTrue("Expected " + me + " <= " + that, me <= that)
	def >>?(that : Long) = assertTrue("Expected " + me + " > " + that, me > that)
	def >=?(that : Long) = assertTrue("Expected " + me + " >= " + that, me >= that)
}

/** Test assertions on Doubles. */
class TestDoubleOps(me : Double) {
	def ==?(that : Double)(implicit e : Epsilon) =
		assertTrue("Expected " + me + " == " + that, math.abs(me - that) < e.v)
	def !=?(that : Double)(implicit e : Epsilon) =
		assertTrue("Expected " + me + " != " + that, math.abs(me - that) > e.v)
	def <<?(that : Double) = assertTrue("Expected " + me + " < " + that, me < that)
	def <=?(that : Double) = assertTrue("Expected " + me + " <= " + that, me <= that)
	def >>?(that : Double) = assertTrue("Expected " + me + " > " + that, me > that)
	def >=?(that : Double) = assertTrue("Expected " + me + " >= " + that, me >= that)
}

/** Test assertions on Booleans. */
class TestBooleanOps(me : Boolean) {
	def ==?(that : Boolean) = assertFalse(me ^ that)
	def !=?(that : Boolean) = assertTrue(me ^ that)

	def ?? = assertTrue(me)
	def !? = assertFalse(me)
}

// vim:sw=4:ts=4:noet
