package structures

object Props:
  type Prop = Boolean
  extension (p: Prop)
    def ==>(q: Prop): Prop = (p <= q)
    def /\ (q: Prop): Prop = (p && q)
    def \/ (q: Prop): Prop = (p || q)
    def ⟶(q: Prop): Prop = (p <= q)
    def =~(q: Prop): Prop = (p == q)
