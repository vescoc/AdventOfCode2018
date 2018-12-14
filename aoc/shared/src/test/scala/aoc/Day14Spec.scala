package aoc

import org.scalatest.{MustMatchers, WordSpec}

import aoc.Day14._

class Day14Spec extends WordSpec with MustMatchers {
  val (takeAfter, findFirstRecurrence) = recipesMaker()

  "part 1 recipes" must {
    def testTakeAfter(input: String, after: Int) =
      s"equals $input after $after" in {
        takeAfter(10, after) mustBe (input)
      }

    testTakeAfter("5158916779", 9)
    testTakeAfter("0124515891", 5)
    testTakeAfter("9251071085", 18)
    testTakeAfter("5941429882", 2018)
  }

  "part 2" must {
    def testFindFirstRecurrence(input: String, index: Int) =
      s"$input appears after $index recipes" in {
        findFirstRecurrence(input) mustBe (index)
      }

    testFindFirstRecurrence("51589", 9)
    testFindFirstRecurrence("01245", 5)
    testFindFirstRecurrence("92510", 18)
    testFindFirstRecurrence("59414", 2018)
  }
}
