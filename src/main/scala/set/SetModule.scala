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
          case ((acc, ts), (e, _)) =>
            if (ts.contains(e)) (acc + (e -> ()), ts)
            else (acc, ts)
        }
        ._1)

  def difference[A](s: Set[A], t: Set[A]): Set[A] =
    Set(
      t.items
        .foldLeft((s.items, s.items): (Map[A, Unit], Map[A, Unit])) {
          case ((acc, ss), (e, _)) =>
            if (ss.contains(e)) (acc - e, ss)
            else (acc, ss)
        }
        ._1)

  def isSubset[A](s: Set[A], t: Set[A]): Boolean =
    s.items.forall { case (e, _) => isMember(e, t) }

  def isMember[A](a: A, s: Set[A]): Boolean =
    s.items.contains(a)

  def isEqual[A](s: Set[A], t: Set[A]): Boolean =
    s.items == t.items
}
