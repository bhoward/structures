package structures

object BoolSyntax:
  extension (p: Boolean)
    def ==>(q: => Boolean): Boolean = (p <= q)
    def /\ (q: => Boolean): Boolean = (p && q)
    def \/ (q: => Boolean): Boolean = (p || q)
    def ===(q: => Boolean): Boolean = (p == q)
