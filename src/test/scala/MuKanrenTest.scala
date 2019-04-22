import MuKanren._
import org.scalatest._

class MuKanrenTest extends FlatSpec with Matchers {

  "State" should "create variables" in {
    val (s, a, b) = State().createVariables("a", "b")
    s.variables should be(List(a, b))
  }

  "State" should "automatically assign unique ids to variables" in {
    val (s, a, b) = State().createVariables("a", "b")
    s.variables.toSet should be(Set(Variable("a", 0), Variable("b", 1)))
  }

  "State" should "be able to assign variables" in {
    val (s, a) = State().createVariables("a")
    s.assignVariables(a -> Integer(2)).values should be(Map(a -> Integer(2)))
  }

  "State" should "be able to extend itself when assigned" in {
    val (s, a, b) = State().createVariables("a", "b")
    s.assignVariables(a -> Integer(2)).assignVariables(b -> Integer(3)).values should be(Map(a -> Integer(2), b -> Integer(3)))
  }

  "State" should "unify terms" in {
    val (s, x, y) = State().createVariables("x", "y")
    s.unify(x, x).get.values should be(Map())
    val maybeState = s.unify(x, y)
    maybeState.map(_.values) should be(Some(Map(x -> y)))

    val unifiedWith5 = maybeState.get.unify(y, Integer(5))
    unifiedWith5.map(_.values) should be(Some(Map(x -> y, y -> Integer(5))))
    unifiedWith5.get.valueOf(x) should be(Integer(5))
  }

  "State" should "not unify terms that occur in each other" in {
    val (s, x) = State().createVariables("x")
    val res = s.unify(x, Pair(x, Integer(3)))
    res should be(None)

    val (s1, p, q, r) = State().createVariables("p", "q", "r")
    val res1 = s1.assignVariables(q -> Pair(Integer(22), Pair(Integer(88), r)), r -> Pair(p, Integer(123))).unify(p, q)
    res1 should be(None)
  }

  "State" should "not unify integers with other integers" in {
    val (state, x) = State().createVariables("x")
    val s = state.assignVariables(x -> Integer(5))
    s.unify(x, Integer(6)) should be(None)

    val (s1, y) = s.createVariables("y")
    s1.assignVariables(y -> x).unify(y, Integer(6)) should be(None)
  }

  "Goal.eq" should "return a state with unifed terms" in {
    val (state, x) = State().createVariables("x")
    Goal.eq(x, Integer(5)).pursueIn(state).map(_.values) should contain (Map(x -> Integer(5)))
  }

  "Goal.eq" should "return an empty Stream if equality doesn't hold" in {
    val (s, x) = State().createVariables("x")
    val st = s.assignVariables(x -> Integer(5))

    Goal.eq(x, Integer(6)).pursueIn(st) should be(Stream())
  }

  "Goal.eq" should "unify pairs" in {
    val eqPair = Goal.fresh { x =>
      Goal.fresh { y =>
        Goal.eq(
          Pair(Integer(3), x),
          Pair(y, Pair(y, Integer(2)))
        )
      }
    }
    val values = eqPair.pursueIn(State()).head.values.values


    values should contain(Pair(Integer(3), Integer(2)))
    values should contain(Integer(3))
    values.size should be(2)
  }

  "Goal.either" should "return should two states if both holds" in {
    val (s, x, y) = State().createVariables("x", "y")
    val g = Goal.either(Goal.eq(x, Integer(5)), Goal.eq(y, Integer(6)))
    val stateValues = g.pursueIn(s).map(_.values)
    stateValues should contain (Map(x -> Integer(5)))
    stateValues should contain (Map(y -> Integer(6)))
    stateValues.size should be (2)
  }

  "Goal.either" should "return only one state if only one holds" in {
    val (s, x, y) = State().createVariables("x", "y")
    val g = Goal.either(Goal.eq(x, Integer(5)), Goal.eq(y, Integer(6)))
    val states = g.pursueIn(s.assignVariables(x -> Integer(6)))
    val stateValues = states.map(_.values)
    stateValues should contain (Map(x -> Integer(6), y -> Integer(6)))
    stateValues.size should be (1)
  }

  "Goal.either" should "return no states if none holds" in {
    val (s, x, y) = State().createVariables("x", "y")
    val finalState = s.assignVariables(x -> Integer(0), y -> Integer(0))
    val g = Goal.either(Goal.eq(x, Integer(5)), Goal.eq(y, Integer(6)))
    val states = g.pursueIn(finalState)
    states.size should be (0)
  }

  "Goal.both" should "combine two goals" in {
    val (s, x, y) = State().createVariables("x", "y")
    val g = Goal.both(Goal.eq(x, Integer(5)), Goal.eq(y, Integer(6)))

    val states = g.pursueIn(s)
    states.size should be (1)

    val state = states.head
    state.values should be (Map(x -> Integer(5), y -> Integer(6)))
  }

  "Goal.both" should "pursue subsequent goals in each of the first goal's states" in {
    val (st, x, y, z, a) = State().createVariables("x", "y", "z", "a")
    val g = Goal.both(
              Goal.either(
                Goal.eq(x, Integer(5)),
                Goal.eq(z, Integer(7))),
              Goal.either(
                Goal.eq(y, Integer(7)),
                Goal.eq(a, Integer(88))))

    val stateValues = g.pursueIn(st).map(_.values)
    stateValues.size should be (4)
    stateValues should contain (Map(x -> Integer(5), y -> Integer(7)))
    stateValues should contain (Map(x -> Integer(5), a -> Integer(88)))
    stateValues should contain (Map(z -> Integer(7), y -> Integer(7)))
    stateValues should contain (Map(z -> Integer(7), a -> Integer(88)))
  }

  "Goal.both" should "return an empty stream if both goals do not hold simultaneously" in {
    val (s, x) = State().createVariables("x")
    val g = Goal.both(
          Goal.eq(x, Integer(5)),
          Goal.eq(Integer(6), x))

    val states = g.pursueIn(s)
    states.size should be (0)
  }

  "MyList" should "produce a cons list" in {
    val l = MyList(Integer(1), Integer(2), Integer(3))
    l should be (Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), Nil))))
  }

  "appendo" should "append an empty list" in {
    val append12 = Goal.fresh { x => appendo(MyList(Integer(1), Integer(2)), MyList(), x) }
    val states = append12.pursueIn(State())
    states.head.results(1) should be(List(MyList(Integer(1), Integer(2))))
  }

  "appendo" should "return all possible inputs to get the given appended list" in {
    val otherWay = Goal.fresh { x =>
      Goal.fresh { y =>
        appendo(x, y, MyList(Integer(1), Integer(2), Integer(3), Integer(4)))
      }
    }
    val results = otherWay.pursueIn(State()).map(_.results(2))
    results should contain(List(Nil, MyList(Integer(1), Integer(2), Integer(3), Integer(4))))
    results should contain(List(MyList(Integer(1)), MyList(Integer(2), Integer(3), Integer(4))))
    results should contain(List(MyList(Integer(1), Integer(2)), MyList(Integer(3), Integer(4))))
    results should contain(List(MyList(Integer(1), Integer(2), Integer(3)), MyList(Integer(4))))
    results should contain(List(MyList(Integer(1), Integer(2), Integer(3), Integer(4)), Nil))
  }

  "reverseo" should "reverse a list" in {
    val reverse123 = Goal.fresh {x => reverseo(MyList(Integer(1), Integer(2), Integer(3)), x)}
    val states = reverse123.pursueIn(State())
    states.head.results(1) should be(List(MyList(Integer(3), Integer(2), Integer(1))))
  }

  "reverseo" should "relate the output with its input" in {
    val goal = Goal.fresh {x => reverseo(MyList(Integer(1), Integer(2), Integer(3)), MyList(Integer(3), Integer(2), x))}
    val states = goal.pursueIn(State())
    states.head.results(1) should be(List(Integer(1)))
  }
}
