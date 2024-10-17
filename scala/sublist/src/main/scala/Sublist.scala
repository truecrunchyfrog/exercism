enum Sublist:
    case Equal
    case Unequal
    case Sublist
    case Superlist

object Sublist:
    def sublist[A](a: Seq[A], b: Seq[A]): Sublist =
        if (a == b) Equal
        else if (b.containsSlice(a)) Sublist
        else if (a.containsSlice(b)) Superlist
        else Unequal