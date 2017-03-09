package set

object SetModule {

  case class Set[A](items: Map[A, Unit])

  def emptySet[A]: Set[A] = Set(Map.empty[A, Unit])

  def union[A](s: Set[A], t: Set[A]): Set[A] =
    Set(s.items ++ t.items)

  def intersection[A](s: Set[A], t: Set[A]): Set[A] =
    Set(
      s.items
        .foldLeft((Map(), t.items): (Map[A, Unit], Map[A, Unit])) {
          case ((result, other), (elem, _)) =>
            if (other.contains(elem)) (result + (elem -> ()), other)
            else (result, other)
        }
        ._1)

  def difference[A](s: Set[A], t: Set[A]): Set[A] =
    Set(
      t.items
        .foldLeft((s.items, s.items): (Map[A, Unit], Map[A, Unit])) {
          case ((result, set), (elem, _)) =>
            if (set.contains(elem)) (result - elem, set)
            else (result, set)
        }
        ._1)

  def isSubset[A](s: Set[A], t: Set[A]): Boolean =
    difference(s, t).items.isEmpty

  def isMember[A](a: A, s: Set[A]): Boolean =
    s.items.contains(a)

  def isEqual[A](s: Set[A], t: Set[A]): Boolean =
    s.items == t.items
}
