// Minitest (c) 2011 Jamie Webb
// MIT License

package cc.jwebb.minitest

import scala.collection.{mutable => M}

class MockTest {
	@Test
	def testMock1 {
		val mockIterator = mock[Iterator[String]]
		mockIterator.hasNext _ -*> false   // Default result
		mockIterator.hasNext _ -#> true    // First result
		mockIterator.hasNext _ -#> true    // Second result
		mockIterator.next _    -#> "First"
		mockIterator.next _    -#> "Second"

		mustThrow[AssertionError] {
			Mock.verify(mockIterator)
			println("Here!")
		}

		val res = new M.ArrayBuffer[String]
		while(mockIterator.hasNext)
			res += mockIterator.next

		Mock.methodNames(mockIterator) ==?
				List("hasNext", "next", "hasNext", "next", "hasNext")

		res.toList ==?
				List("First", "Second")

		val hn = (mockIterator.hasNext _).method
		val n = (mockIterator.next _).method

		Mock.records(mockIterator) ==?
				List(
					Mock.Rec(mockIterator, hn, 0, Nil, true),
					Mock.Rec(mockIterator, n, 0, Nil, "First"),
					Mock.Rec(mockIterator, hn, 1, Nil, true),
					Mock.Rec(mockIterator, n, 1, Nil, "Second"),
					Mock.Rec(mockIterator, hn, 2, Nil, false))

		Mock.verify(mockIterator)

		mockIterator.next
		mustThrow[AssertionError] {
			Mock.verify(mockIterator)
		}
	}

	@Test
	def testMock2 {
		val mockSet = mock[java.util.Set[Int]]

		// Return true first time, otherwise default to false
		mockSet.contains _   -#> true

		// But return true for this specific argument
		mockSet.contains(23) -!> true

		(mockSet contains 5).??
		(mockSet contains 5).!?
		(mockSet contains 9).!?
		(mockSet contains 23).??
		(mockSet contains 23).!?

		// Different ways to check the results
		Mock.methodNames(mockSet).length ==?
				5

		Mock.records(mockSet).flatMap(r => r.args).toSet ==?
				Set(5, 9, 23)

		(mockSet.contains _).allArgs.flatten ==?
				List(5, 5, 9, 23, 23)

		// Check we didn't call toString, but be lenient otherwise
		verify(mockSet, Set(mockSet.toString _))

		mockSet.toString  // Ok, now we did...
		mustThrow[AssertionError] {
			verify(mockSet, Set(mockSet.toString _))
		}
	}
}

// vim:sw=4:ts=4:noet
