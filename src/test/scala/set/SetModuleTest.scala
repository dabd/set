package set

import org.scalacheck._

class SetModuleTest extends CommonSpec {

  import SetModule._

  def genSet(range: Range = Range(1, 50),
             maxSize: Option[Int] = None): Gen[Set[Int]] = {

    val gen = for {
      genInt <- Gen.choose(range.start, range.end)
      genUnit <- Arbitrary.arbUnit.arbitrary
    } yield (genInt, genUnit)

    for {
      items <- maxSize match {
        case None => Gen.mapOf[Int, Unit](gen)
        case Some(n) =>
          Gen.choose(0, n) flatMap { sz =>
            Gen.mapOfN[Int, Unit](sz, gen)
          }
      }
    } yield Set(items)
  }

  "union" should {
    "be associative" in {
      forAll(genSet(), genSet(), genSet()) {
        case (s, t, u) => union(union(s, t), u) shouldBe union(s, union(t, u))
      }
    }
    "be commutative" in {
      forAll(genSet(), genSet()) {
        case (s, t) => union(s, t) shouldBe union(t, s)
      }
    }

    "be distributive relative to intersection" in {
      forAll(genSet(), genSet(), genSet()) {
        case (s, t, u) =>
          union(s, intersection(t, u)) shouldBe intersection(union(s, t),
            union(s, u))
      }
    }

    "have the empty set as identity" in {
      forAll(genSet()) { s =>
        union(s, SetModule.emptySet) shouldBe s
      }
    }
    "be idempotent: union(s, s) = s" in {
      forAll(genSet()) { s =>
        union(s, s) shouldBe s
      }
    }

    "be like scala.collection.immutable.Set union" in {
      forAll(genSet(), genSet()) {
        case (s, t) => union(s, t).items.keySet shouldBe s.items.keySet.union(t.items.keySet)
      }
    }
  }

  "intersection" should {
    "be associative" in {
      forAll(genSet(), genSet(), genSet()) {
        case (s, t, u) =>
          intersection(intersection(s, t), u) shouldBe intersection(
            s,
            intersection(t, u))
      }
    }
    "be commutative" in {
      forAll(genSet(), genSet()) {
        case (s, t) => intersection(s, t) shouldBe intersection(t, s)
      }
    }

    "be distributive relative to union" in {
      forAll(genSet(), genSet(), genSet()) {
        case (s, t, u) =>
          intersection(s, union(t, u)) shouldBe union(intersection(s, t),
            intersection(s, u))
      }
    }

    "of a set with itself should the same set" in {
      forAll(genSet()) { s =>
        intersection(s, s) shouldBe s
      }
    }

    "be like scala.collection.immutable.Set intersect" in {
      forAll(genSet(), genSet()) {
        case (s, t) => intersection(s, t).items.keySet shouldBe s.items.keySet.intersect(t.items.keySet)
      }
    }
  }

  "difference" should {
    // https://en.wikipedia.org/wiki/Complement_(set_theory)

    "C - (A && B) = (C - A) || (C - B)" in {
      forAll(genSet(), genSet(), genSet()) {
        case (c, a, b) =>
          difference(c, intersection(a, b)) shouldBe union(difference(c, a),
            difference(c, b))
      }
    }

    " C - (A || B) = (C - A) && (C - B)" in {
      forAll(genSet(), genSet(), genSet()) {
        case (c, a, b) =>
          difference(c, union(a, b)) shouldBe intersection(difference(c, a),
            difference(c, b))
      }
    }

    "C - (B - A) = (C && A) || (C - B)" in {
      forAll(genSet(), genSet(), genSet()) {
        case (c, b, a) =>
          difference(c, difference(b, a)) shouldBe union(intersection(c, a),
            difference(c, b))
      }
    }

    "(B - A) && C = (B && C) - A = B && (C - A)" in {
      forAll(genSet(), genSet(), genSet()) {
        case (b, a, c) =>
          intersection(difference(b, a), c) should
            (equal(difference(intersection(b, c), a))
              and equal(intersection(b, difference(c, a))))

      }
    }

    "(B - A) || C = (B || C) - (A - C)" in {
      forAll(genSet(), genSet(), genSet()) {
        case (b, a, c) =>
          union(difference(b, a), c) shouldBe
            difference(union(b, c), difference(a, c))
      }
    }

    "A - A = empty" in {
      forAll(genSet()) { a =>
        difference(a, a) shouldBe emptySet
      }
    }

    "empty - A = empty" in
      forAll(genSet()) { a =>
        difference(emptySet, a) shouldBe emptySet
      }

    "A - empty = A" in {
      forAll(genSet()) { a =>
        difference(a, emptySet) shouldBe a
      }
    }

    "be like scala.collection.immutable.Set diff" in {
      forAll(genSet(), genSet()) {
        case (s, t) => difference(s, t).items.keySet shouldBe s.items.keySet.diff(t.items.keySet)
      }
    }
  }

  "isSubset" should {

    "the intersection of two sets should be a subset of both sets" in {
      forAll(genSet(), genSet()) {
        case (s, t) =>
          isSubset(intersection(s, t), s) shouldBe true
          isSubset(intersection(s, t), t) shouldBe true
      }
    }

    "the difference of two sets should not be a subset of the second set" in {
      forAll(genSet() suchThat(_.items.nonEmpty), genSet()) {
        case (s, t) =>
          isSubset(difference(s, t), t) shouldBe false
      }
    }

    "be like scala.collection.immutable.Set subsetOf" in {
      forAll(genSet(Range(1, 20), Some(5)), genSet(Range(1, 20), Some(8))) {
        case (s, t) =>
          isSubset(s, t) shouldBe s.items.keySet.subsetOf(t.items.keySet)
      }
    }
  }

  "isMember" should {

    "every member of the intersection of two sets should be a member of both sets" in {
      forAll(genSet(), genSet()) {
        case (s, t) =>
          intersection(s, t).items.forall { case (e, _) => isMember(e, s) } shouldBe true
          intersection(s, t).items.forall { case (e, _) => isMember(e, t) } shouldBe true
      }
    }

    "every member of the difference between two sets should not be a member of the second set" in {
      forAll(genSet() suchThat(_.items.nonEmpty), genSet()) {
        case (s, t) =>
          difference(s, t).items.forall { case (e, _) => isMember(e, t) } shouldBe false
      }
    }

    "be like scala.collection.immutable.Set contains" in {
      forAll(genSet(Range(1, 20)), Gen.choose(1, 40)) {
        case (s, e) => isMember(e, s) shouldBe s.items.keySet.contains(e)
      }
    }
  }

  "isEqual" should {
    "return true if two sets are equal" in {
      forAll(genSet()) { s =>
        isEqual(s, s) shouldBe true
      }
    }

    "return false if two sets are not equal" in {
      forAll(genSet(Range(1, 10)) suchThat (_.items.nonEmpty),
        genSet(Range(20, 30))) {
        case (s, t) => isEqual(s, t) shouldBe false
      }
    }

    "be like scala.collection.immutable.Set ==" in {
      forAll(genSet(), genSet()) {
        case (s, t) => isEqual(s, t) shouldBe s.items.keySet == t.items.keySet
      }
    }
  }

}
