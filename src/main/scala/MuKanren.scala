object MuKanren {

  sealed trait Term
  case class Variable(name: String, id: Int) extends Term {
    override def toString: String = name + id
  }
  case class Integer(value: Int) extends Term {
    override def toString: String = value.toString
  }
  case class Pair(first: Term, second: Term) extends Term {
    override def toString: String = s"($first, $second)"
  }
  case object Nil extends Term

  // Replace this after we have upgraded to scala 2.13
  type LazyList[T] = Stream[T]
  val LazyList = Stream

  trait Goal {

    def doit(state: State): LazyList[State]

    def pursueIn(state: State): LazyList[State] = doit(state)

  }

  object Goal {

    def eq(x: Term, y: Term): Goal = state => state.unify(x, y).toStream

    def either(first: Goal, second: Goal): Goal = state => {
      val f = first.pursueIn(state)
      val s = second.pursueIn(state)
      f ++ s
    }

    def both(first: Goal, second: Goal): Goal = state => {
      val states = first.pursueIn(state)
      states.flatMap(s => second.pursueIn(s))
    }

    def fresh(block: Variable => Goal): Goal = {
      state => {
        val (st, v) = state.createAnonymousVariable()
        val goal = block(v)
        goal.pursueIn(st)
      }
    }

    def fresh(block: (Variable, Variable) => Goal): Goal = {
      state => {
        val (s1, v1) = state.createAnonymousVariable()
        val (s2, v2) = s1.createAnonymousVariable()
        val goal = block(v1, v2)
        goal.pursueIn(s2)
      }
    }

    def fresh(block: (Variable, Variable, Variable) => Goal): Goal = {
      state => {
        val (s1, v1) = state.createAnonymousVariable()
        val (s2, v2) = s1.createAnonymousVariable()
        val (s3, v3) = s2.createAnonymousVariable()
        val goal = block(v1, v2, v3)
        goal.pursueIn(s3)
      }
    }
  }

  object MyList {
    def apply(values: Term*): Term = {
      def inner(vals: List[Term]): Term = {
        vals match {
          case hd :: tl => Pair(hd, inner(tl))
          case _ => Nil
        }
      }

      inner(values.toList)
    }
  }

  class State(val variables: Seq[Variable], val values: Map[Term, Term], var counter: Int = 0) {

    val freshVar: String => Variable = {
      { name: String =>
        val res = Variable(name, counter)
        counter = counter + 1
        res
      }
    }

    def createAnonymousVariable(): (State, Variable) = {
      val v1 = freshVar("v")
      (State(this.variables :+ v1, this.values, this.counter), v1)
    }

    def createVariables(variables: Variable*): State = State(this.variables ++ variables, this.values, this.counter)

    def createVariables(var1: String): (State, Variable) = {
      val v1 = freshVar(var1)
      (State(this.variables :+ v1, this.values, this.counter), v1)
    }

    def createVariables(var1: String, var2: String): (State, Variable, Variable) = {
      val v1 = freshVar(var1)
      val v2 = freshVar(var2)
      (State(this.variables :+ v1 :+ v2, this.values, this.counter), v1, v2)
    }

    def createVariables(var1: String, var2: String, var3: String): (State, Variable, Variable, Variable) = {
      val v1 = freshVar(var1)
      val v2 = freshVar(var2)
      val v3 = freshVar(var3)
      (State(this.variables :+ v1 :+ v2 :+ v3, this.values, this.counter), v1, v2, v3)
    }

    def createVariables(var1: String, var2: String, var3: String, var4: String): (State, Variable, Variable, Variable, Variable) = {
      val v1 = freshVar(var1)
      val v2 = freshVar(var2)
      val v3 = freshVar(var3)
      val v4 = freshVar(var4)
      (State(this.variables :+ v1 :+ v2 :+ v3, this.values, this.counter), v1, v2, v3, v4)
    }

    def assignVariables(elems: (Variable, Term)*): State = State(this.variables, this.values ++ Map(elems: _*), this.counter)

    def valueOf(candidate: Term): Term = {
      candidate match {
        case _: Variable => values.get(candidate).map(valueOf).getOrElse(candidate)
        case Pair(f, s) => Pair(valueOf(f), valueOf(s))
        case _ => candidate
      }
    }

    def unify(x: Term, y: Term): Option[State] = {
      val (xVal, yVal) = (valueOf(x), valueOf(y))

      (xVal, yVal) match {
        case _ if xVal == yVal => Some(this)
        case (xVal: Variable, _) => Some(assignVariables(xVal -> yVal))
        case (_, yVal: Variable) => Some(assignVariables(yVal -> xVal))
        case (Pair(f1, s1), Pair(f2, s2)) => unify(f1, f2).flatMap(state => state.unify(s1, s2))
        case _ => None
      }
    }

    def results(n: Int): Seq[Term] = variables.take(n).map(valueOf)
  }

  object State {
    def apply(): State = new State(Seq.empty, Map.empty)

    def apply(variables: Seq[Variable], values: Map[Term, Term], counter: Int): State = new State(variables, values, counter)
  }

  def appendo(a: Term, b: Term, c: Term): Goal = {
    Goal.either(
      Goal.both(
        Goal.eq(a, Nil),
        Goal.eq(b, c)
      ),
      Goal.fresh { (first, restA, restC) =>
        Goal.both(
          Goal.both(
            Goal.eq(a, Pair(first, restA)),
            Goal.eq(c, Pair(first, restC))),
          appendo(restA, b, restC)
        )
      }
    )
  }
}

