// Minitest (c) 2011 Jamie Webb
// MIT License

package cc.jwebb

package object minitest {
	/** Main conversion for test assertions on references. */
	implicit def testObjectOps[T <: AnyRef](x : T) = new TestObjectOps(x)
	/** Main conversion for test assertions on Byte/Char/Short/Int/Long. */
	implicit def testLongOps(x : Long) = new TestLongOps(x)
	/** Main conversion for test assertions on Float/Double. */
	implicit def testDoubleOps(x : Double) = new TestDoubleOps(x)
	/** Main conversion for test assertions on Boolean. */
	implicit def testBooleanOps(x : Boolean) = new TestBooleanOps(x)

	type Test = org.junit.Test
	def fail() = org.junit.Assert.fail()
	def fail(m : String) = org.junit.Assert.fail(m)
	def need(b : Boolean) = org.junit.Assert.assertTrue(b)

	/** Tests that the given thunk throws the given exception. Fails otherwise. */
	def mustFail[T <: Throwable](thunk : => Unit)(implicit m : Manifest[T]) {
		var failed = true
		try {
			thunk
			failed = false
		} catch {
			case e =>
				if (!m.erasure.isAssignableFrom(e.getClass))
					throw e
		}
		if (!failed)
			fail("Expected " + m.erasure.getName)
	}

	/** Creates a new mock of the given interface. */
	def mock[T](implicit m : Manifest[T]) : T = Mock.mock[T](m)

	// Conveniently, Scala appears to automatically convert nulls into
	// default primitives where required, so the below casts will work even for
	// primive arguments.
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function0[Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function0[Object]]
				()).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function1[Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function1[Object, Object]]
				(null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function2[Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function2[Object, Object, Object]]
				(null, null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function3[Nothing, Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function3[Object, Object, Object, Object]]
				(null, null, null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function4[Nothing, Nothing, Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function4[Object, Object, Object, Object, Object]]
				(null, null, null, null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function5[Nothing, Nothing, Nothing, Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function5[Object, Object, Object, Object, Object, Object]]
				(null, null, null, null, null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function6[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function6[Object, Object, Object, Object, Object, Object, Object]]
				(null, null, null, null, null, null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function7[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function7[Object, Object, Object, Object, Object, Object, Object, Object]]
				(null, null, null, null, null, null, null)).any
	/** Returns the any-call record for this method closure */
	implicit def method(f : Function8[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Any]) : Mock.AnyRec =
		Mock.call(f.asInstanceOf[Function8[Object, Object, Object, Object, Object, Object, Object, Object, Object]]
				(null, null, null, null, null, null, null, null)).any

	/** Actually creates a Rec, but returns ExactArrow just so the implicit is
	 * only triggered by the arrow operator, because otherwise it would catch
	 * everything. */
	implicit def exactArrow(thunk : => Any) : Mock.ExactArrow =
		Mock.call(thunk)
}

// vim:sw=4:ts=4:noet
