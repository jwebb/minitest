// Minitest (c) 2011 Jamie Webb
// MIT License

package cc.jwebb.minitest

import java.{lang => J}
import java.lang.reflect.{Method, Proxy, InvocationHandler}
import java.util.IdentityHashMap
import scala.collection.{mutable => M}

/** Mocking framework using lots of Scala goodness. See MockTest for example
 * usage. */
object Mock {
	private val lock = new Object

	/** Pulled out as a supertype of Rec so that the exactArrow implicit isn't
	 * triggered too readily. */
	trait ExactArrow {
		/** If this method is seen once with these exact arguments, return the
		 * given value. */
		def -!>(r : Any) : Unit
	}

	/** Represents a record of a particular call to a method of a mock instance,
	 * complete with parameters and result. The 'count' member is the number of
	 * calls already made to this method of this instance. */
	case class Rec(proxy : Object, method : Method, count : Int,
			args : List[Any], result : Any) extends ExactArrow {
		def -!>(r : Any) =
			lock.synchronized {
				val m = exactMap(proxy)
				m.get(method) match {
					case Some(mm) =>
						mm(args) = r
					case None =>
						val mm = new M.HashMap[List[Any], Any]
						mm(args) = r
						m(method) = mm
				}
			}

		/** Returns an any-call record for this proxy and method. */
		def any = AnyRec(proxy, method)

		// Heisenburg frowns upon us calling toString on our mock instance...
		override def toString() =
			count + ": <" + names.get(proxy) + "@" +
					System.identityHashCode(proxy) + ">." + method.getName + "(" +
					args.mkString(", ") + ") -> " + result

		// ...or hashCode...
		override def hashCode() =
			System.identityHashCode(proxy) + (method, count, args, result).hashCode

		// ...or equals.
		override def equals(that : Any) =
			if (this eq that.asInstanceOf[Object]) {
				true
			} else if (that.isInstanceOf[Rec]) {
				val r = this.asInstanceOf[Rec]
				(proxy eq r.proxy) && method == r.method &&
						count == r.count && args == r.args &&
						result == r.result
			} else {
				false
			}
	}

	/** Rpresents a call to a particular method of a mock instance, but without
	 * specifying what parameters are passed. */
	case class AnyRec(proxy : Object, method : Method) {
		/** If this method is called, return the given value. */
		def -*>(r : Any) =
			lock.synchronized {
				val m = allMap(proxy)
				m(method) = r
			}

		/** Add to the sequence of return values for this method. */
		def -#>(r : Any) =
			lock.synchronized {
				val m = posMap(proxy)
				m.get(method) match {
					case Some(a) => a += r
					case None =>
						val a = new M.ArrayBuffer[Any]
						a += r
						m(method) = a
				}
			}

		/** Returns all records that match this proxy and method. */
		def allCalls : List[Rec] =
			hist(proxy).view.filter(_.method == method).toList

		/** Returns all argument lists for this proxy and method. */
		def allArgs : List[List[Any]] =
			allCalls.map(_.args)

		/** Returns the number of times this method has been called so far. */
		def count : Int = allCalls.length

		// Heisenburg again...
		override def toString() = "<" + names.get(proxy) + "@" +
				System.identityHashCode(proxy) + ">." + method.getName

		override def hashCode() =
			System.identityHashCode(proxy) + method.hashCode

		override def equals(that : Any) =
			if (this eq that.asInstanceOf[Object]) {
				true
			} else if (that.isInstanceOf[AnyRec]) {
				val r = this.asInstanceOf[AnyRec]
				(proxy eq r.proxy) && method == r.method
			} else {
				false
			}
	}

	/** Wraps IdentityHashMap so the key is always a mock instance and missing
	 * values and auto-created. */
	private class IHM[T](mk : => T) extends IdentityHashMap[Object, T] {
		def apply(k : Object) = {
			val v = get(k)
			if (v == null) {
				if (!Proxy.isProxyClass(k.getClass))
					throw new IllegalArgumentException("Target is not a mock instance")
				val n = mk
				put(k, n)
				n
			} else {
				v
			}
		}
	}

	private var hist = new IHM(new M.ArrayBuffer[Rec])
	private var counts = new IHM(new M.HashMap[Method, Int])
	private val posMap = new IHM(new M.HashMap[Method, M.ArrayBuffer[Any]])
	private val exactMap = new IHM(new M.HashMap[Method, M.HashMap[List[Any], Any]])
	private val doneExacts = new IHM(new M.HashMap[Method, Int])
	private val allMap = new IHM(new M.HashMap[Method, Any])
	private val names = new IdentityHashMap[Object, String]

	/** Creates a new mock of the given interface - this is also available as a
	 * package method. */
	def mock[T](implicit m : Manifest[T]) : T = {
		val p = Proxy.newProxyInstance(
				this.getClass.getClassLoader,
				Array(m.erasure),
				new InvocationHandler {
					def invoke(proxy : Object, method : Method,
							args : Array[Object]) = {
						val n = recordCount(proxy, method)
						val r = exact(proxy, method, args) match {
							case Some(r) => r
							case None =>
								pos(n, proxy, method) match {
									case Some(r) => r
									case None =>
										all(proxy, method) match {
											case Some(r) => r
											case None =>
												defaultReturn(method)
										}
								}
						}
						record(proxy, method, n, args, r)
						r.asInstanceOf[Object]
					}
				}).asInstanceOf[T]
		names.put(p.asInstanceOf[Object], m.erasure.getName)
		p
	}

	/** Returns the calls on this proxy, in order. */
	def records(proxy : Object) : List[Rec] =
		lock.synchronized {
			hist(proxy).toList
		}

	/** Returns the names of methods called on this proxy, in order. */
	def methodNames(proxy : Object) : List[String] =
		records(proxy).map(_.method.getName)

	/** Checks that all expected calls were made. If strict is null, also checks
	 * for unexpected calls to any method, otherwise is strict for just the named
	 * methods. */
	def verify(proxy : Object, strict : Iterable[AnyRec] = null) = {
		val mpos = posMap(proxy).mapValues(b => b.length)
		val mall = allMap(proxy)
		val mcounts = counts(proxy)
		val mexacts = exactMap(proxy)
		val mdexacts = doneExacts(proxy)
		val smethods =
			if (strict == null) null
			else strict.toList.filter(_.proxy eq proxy).map(_.method).toSet

		for ((m, c) <- mcounts) {
			if (strict == null || (smethods contains m)) {
				if (!mall.contains(m)) {
					val me = mdexacts.get(m)
					val mc = mpos.get(m)
					val ec = mc.getOrElse(0) + me.getOrElse(0)
					if (ec != c)
						throw new AssertionError(
								"Expected " + ec + " (not " + c + ") calls to " + m)
				}
			}
		}

		for (m <- mall.keys) {
			if (!mcounts.contains(m))
				throw new AssertionError(
						"Declared call to " + m + " not made")
		}

		for ((m, c) <- mpos) {
			if (!mcounts.contains(m))
				throw new AssertionError(
						"Declared " + c + " calls to " + m + " not made")
			if (mcounts(m) < c)
				throw new AssertionError(
						"Expected at least " + c + " (not " + mcounts(m) +
						") calls to " + m)
		}

		for ((m, calls) <- mexacts) {
			for ((a, r) <- calls)
				throw new AssertionError(
						"Missing declared call " + Rec(proxy, m, 0, a, r))
		}
	}

	/** Returns the call record for this thunk. */
	def call(thunk : => Any) : Rec =
		lock.synchronized {
			val oh = hist
			val oc = counts
			try {
				hist = new IHM(new M.ArrayBuffer[Rec])
				counts = new IHM(new M.HashMap[Method, Int])
				thunk
				if (hist.size != 1)
					throw new IllegalArgumentException("Expected one proxy, was " + hist.size)
				val b = hist.values.iterator.next()
				if (b.length != 1)
					throw new IllegalArgumentException("Expected one call, was " + b.length)
				b.head
			} finally {
				hist = oh
				counts = oc
			}
		}

	private def recordCount(proxy : Object, method : Method) =
		lock.synchronized {
			val c = counts(proxy)
			val n = c.getOrElse(method, 0)
			c(method) = n + 1
			n
		}

	private def record(proxy : Object, method : Method, n : Int,
			args : Array[Object], ret : Any) : Unit =
		lock.synchronized {
			val b = hist(proxy)
			val alist = listNoNulls(args)
			b += Rec(proxy, method, n, alist, ret)
		}

	private def pos(n : Int, proxy : Object, method : Method) =
		lock.synchronized {
			posMap(proxy).get(method) match {
				case Some(b) if b.length > n =>
					Some(b(n))
				case _ =>
					None
			}
		}

	// See also Scala bug #4311 - now fixed, but we keep the workaround in for
	// the moment to remain compatible with 2.8.1
	private object NULL { override def toString() = "null" }
	private def listNoNulls(x : Array[Object]) : List[Any] =
		if (x == null) Nil
		else x.toList.map(x => if (x == null) NULL else x)

	private def exact(proxy : Object, method : Method, args : Array[Object]) =
		lock.synchronized {
			val a = listNoNulls(args)
			exactMap(proxy).get(method) match {
				case Some(m) =>
					m.get(a) match {
						case Some(r) =>
							m -= a // Exacts are only valid once
							val d = doneExacts(proxy)
							d(method) = d.getOrElse(method, 0) + 1
							Some(r)
						case None => None
					}
				case None => None
			}
		}

	private def all(proxy : Object, method : Method) =
		lock.synchronized {
			allMap(proxy).get(method)
		}

	private def defaultValue(clazz : Class[_]) : Object =
		clazz match {
			case J.Boolean.TYPE => J.Boolean.FALSE
			case J.Byte.TYPE => J.Byte.valueOf(0.asInstanceOf[Byte])
			case J.Short.TYPE => J.Short.valueOf(0.asInstanceOf[Short])
			case J.Character.TYPE => J.Character.valueOf(0)
			case J.Integer.TYPE => J.Integer.valueOf(0)
			case J.Long.TYPE => J.Long.valueOf(0)
			case J.Float.TYPE => J.Float.valueOf(0)
			case J.Double.TYPE => J.Double.valueOf(0)
			case _ => null
		}

	private def defaultReturn(method : Method) : Object =
		defaultValue(method.getReturnType)

	private def invoke(proxy : Object, method : Method) : Object = {
		val in = method.getParameterTypes.map(defaultValue)
		method.invoke(proxy, in)
	}
}

// vim:sw=4:ts=4:noet
