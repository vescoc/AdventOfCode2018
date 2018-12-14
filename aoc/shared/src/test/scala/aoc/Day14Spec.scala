package aoc

import org.scalatest.{MustMatchers, WordSpec}

import aoc.Day14._

class Day14Spec extends WordSpec with MustMatchers {
  "recipesMaker" must {
    "equals 5158916779 with initial 37" in {
      recipesMaker(37).drop(9).take(10).mkString mustBe ("5158916779")
    }

    "equals 0124515891 with initial 37 after 5 recipes" in {
      recipesMaker(37).drop(5).take(10).mkString mustBe ("0124515891")
    }

    "equals 9251071085 with initial 37 after 18 recipes" in {
      recipesMaker(37).drop(18).take(10).mkString mustBe ("9251071085")
    }

    "equals 5941429882 with initial 37 after 2018 recipes" in {
      recipesMaker(37).drop(2018).take(10).mkString mustBe ("5941429882")
    }
  }
}
