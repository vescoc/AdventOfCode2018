package aoc

import scala.annotation.tailrec

object Day24 {
  import State._

  case class State(immuneSystem: List[Group], infection: List[Group]) {
    def getImmuneSystem(id: Int) = getById(immuneSystem, id)
    def getInfection(id: Int) = getById(infection, id)

    def getById(list: List[Group], id: Int) =
      list.filter { _.id == id } match {
        case head :: tail =>
          Some(head)
        case _ =>
          None
      }

    def updateImmuneSystem(group: Group) =
      if (group.units <= 0)
        copy(immuneSystem = immuneSystem.filterNot { _.id == group.id })
      else
        copy(immuneSystem = immuneSystem.map { that => if (that.id == group.id) group else that })

    def updateInfection(group: Group) =
      if (group.units <= 0)
        copy(infection = infection.filterNot { _.id == group.id })
      else
        copy(infection = infection.map { that => if (that.id == group.id) group else that })

    def units() =
      (immuneSystem ++ infection)
        .map { _.units }
        .sum

    override def toString =
      s"""|State:
          |Immune System:
          |${immuneSystem.sortBy { _.id }.mkString("\n")}
          |Infection:
          |${infection.sortBy { _.id }.mkString("\n")}""".stripMargin
  }

  object State {
    val immuneSystemRe = """Immune System:""".r
    val infectionRe = """Infection:""".r
    val groupRe = """(\d+) units each with (\d+) hit points \(([^)]+)\) with an attack that does (\d+) (.+) damage at initiative (\d+)""".r
    val groupSimpleRe = """(\d+) units each with (\d+) hit points with an attack that does (\d+) (.+) damage at initiative (\d+)""".r
    val immuneToRe = """immune to ([^;]+)""".r
    val weakToRe = """weak to ([^;]+)""".r
    val multiSpecRe = """(.+); (.+)""".r

    case class Group(
      id: Int,
      units: Int,
      hitpoints: Int,
      attackDamage: Int,
      attackType: String,
      weaknesses: Set[String],
      immunities: Set[String],
      initiative: Int
    ) {
      lazy val effectivePower = units * attackDamage

      def calcDamageFrom(attacker: Group): Int = {
        val wm = if (weaknesses.contains(attacker.attackType)) 2 else 1
        val im = if (immunities.contains(attacker.attackType)) 0 else 1

        attacker.units * attacker.attackDamage * wm * im
      }

      def defendFrom(attacker: Group): Group = {
        val lostHitpoints = calcDamageFrom(attacker)

        val remainder = units - lostHitpoints / hitpoints

        copy(units = if (remainder < 0) 0 else remainder)
      }
    }

    def immuneSystemVsInfection(
      currentState: State,
      nextState: State,
      attackingGroupId: Int,
      defendingGroupId: Int
    ): State =
      (currentState.getImmuneSystem(attackingGroupId), currentState.getInfection(defendingGroupId)) match {
        case (Some(ag), Some(dg)) =>
          val ndg = dg.defendFrom(ag)
          nextState.updateInfection(ndg)
        case _ =>
          nextState
      }

    def infectionVsImmuneSystem(
      currentState: State,
      nextState: State,
      attackingGroupId: Int,
      defendingGroupId: Int
    ): State =
      (currentState.getInfection(attackingGroupId), currentState.getImmuneSystem(defendingGroupId)) match {
        case (Some(ag), Some(dg)) =>
          val ndg = dg.defendFrom(ag)
          nextState.updateImmuneSystem(ndg)
        case _ =>
          nextState
      }    

    def round(state: State): State =
      (targetSelection andThen attack)(state)

    @tailrec
    def game(state: State): State = {
      if (state.immuneSystem.isEmpty || state.infection.isEmpty)
        state
      else
        game(round(state))
    }

    type TargetSelectionDamage = ((Group, Group), Int)

    case class AttackAction(attackingGroup: Group, defendingGroup: Group, action: (State, State, Int, Int) => State) extends Comparable[AttackAction] {
      def run(currentState: State, nextState: State) = action(currentState, nextState, attackingGroup.id, defendingGroup.id)

      def compareTo(that: AttackAction): Int = attackingGroup.initiative compare that.attackingGroup.initiative
    }

    case class TargetSelection(immuneSystem: List[AttackAction], infection: List[AttackAction]) {
      def toList(state: State): List[AttackAction] = ???
    }

    object GroupEffectivePowerOrdering extends Ordering[Group] {
      def compare(b: Group, a: Group) = {
        val c = a.effectivePower compare b.effectivePower
        if (c != 0)
          c
        else
          a.initiative compare b.initiative
      }
    }

    object GroupAttackingSelectionOrdering extends Ordering[(Group, Int)] {
      def compare(b: (Group, Int), a: (Group, Int)) = {
        val c = a._2 compare b._2
        if (c != 0)
          c
        else {
          val d = a._1.effectivePower compare b._1.effectivePower
          if (d != 0)
            d
          else
            a._1.initiative compare b._1.initiative
        }
      }
    }

    lazy val targetSelection: PartialFunction[State, (State, TargetSelection)] = {
      def targetSelectionFrom(attackingGroup: List[Group], defendingGroup: List[Group], action: (State, State, Int, Int) => State): List[AttackAction] = {
        val l = attackingGroup.sorted(GroupEffectivePowerOrdering)

        @tailrec
        def targetSelectionFromR(attackingGroups: List[Group], defendingGroups: List[Group], result: List[TargetSelectionDamage]): List[TargetSelectionDamage] =
          if (defendingGroups.isEmpty)
            result
          else
            attackingGroups match {
              case attackingGroup :: tail =>
                val defendingGroup = defendingGroups
                  .map { defendingGroup =>
                    defendingGroup -> defendingGroup.calcDamageFrom(attackingGroup)
                  }
                  .sorted(GroupAttackingSelectionOrdering)
                  .head

                if (defendingGroup._2 != 0)
                  targetSelectionFromR(
                    tail,
                    defendingGroups.filter { _.id != defendingGroup._1.id },
                    ((attackingGroup -> defendingGroup._1), defendingGroup._2) :: result
                  )
                else
                  targetSelectionFromR(
                    tail,
                    defendingGroups,
                    result
                  )
                  
              case _ =>
                result
            }

        targetSelectionFromR(l, defendingGroup, List.empty)
          .map { i => AttackAction(i._1._1, i._1._2, action) }
      }

      {
        case state: State =>
          (
            state,
            TargetSelection(
              targetSelectionFrom(state.immuneSystem, state.infection, immuneSystemVsInfection),
              targetSelectionFrom(state.infection, state.immuneSystem, infectionVsImmuneSystem)
            )
          )
      }
    }

    lazy val attack: PartialFunction[(State, TargetSelection), State] = {
      {
        case (state, TargetSelection(immuneSystemTargets, infectionTargets)) if !immuneSystemTargets.isEmpty && !infectionTargets.isEmpty =>
          (immuneSystemTargets ++ infectionTargets)
            .sorted(Ordering.ordered[AttackAction])
            .reverse
            .foldLeft(state) { (nextState, attackAction) => attackAction.run(nextState, nextState) }
      }
    }

    def parseSpecs(specs: String): (Set[String], Set[String]) = {
      def split(str: String) = str.split(",").map { _.trim }.toSet

      specs match {
        case multiSpecRe(part1, part2) =>
          (part1, part2) match {
            case (immuneToRe(immunities), weakToRe(weaknesses)) =>
              (split(weaknesses), split(immunities))
            case (weakToRe(weaknesses), immuneToRe(immunities)) =>
              (split(weaknesses), split(immunities))
          }
        case weakToRe(weaknesses) => (split(weaknesses), Set.empty)
        case immuneToRe(immunities) => (Set.empty, split(immunities))
      }
    }

    def parse(lines: Iterator[String]): State = {
      val ids = Map(1 -> Iterator.from(1), 2 -> Iterator.from(1))

      val r = lines
        .foldLeft[(Int, Option[List[Group]], Option[List[Group]], Option[List[Group]])]((0, None, None, None)) { (acc, line) =>
          (acc, line) match {
            case ((0, None, _, None), immuneSystemRe()) =>
              (1, None, acc._2, Some(List.empty))
            case ((0, _, None, None), infectionRe()) =>
              (2, acc._2, None, Some(List.empty))
            case ((_, _, _, Some(l)), groupRe(units, hitpoints, specs, attackDamage, attackType, initiative)) if (acc._1 != 0) =>
              val (weaknesses, immunities) = parseSpecs(specs)

              (
                acc._1,
                acc._2,
                acc._3,
                Some(
                  Group(
                    ids(acc._1).next,
                    units.toInt,
                    hitpoints.toInt,
                    attackDamage.toInt,
                    attackType,
                    weaknesses,
                    immunities,
                    initiative.toInt
                  ) :: l
                )
              )
            case ((_, _, _, Some(l)), groupSimpleRe(units, hitpoints, attackDamage, attackType, initiative)) if (acc._1 != 0) =>
              (
                acc._1,
                acc._2,
                acc._3,
                Some(
                  Group(
                    ids(acc._1).next,
                    units.toInt,
                    hitpoints.toInt,
                    attackDamage.toInt,
                    attackType,
                    Set.empty,
                    Set.empty,
                    initiative.toInt
                  ) :: l
                )
              )
            case ((_, _, _, Some(l)), "") if acc._1 != 0 =>
              acc._1 match {
                case 1 =>
                  (0, Some(l), acc._3, None)
                case 2 =>
                  (0, acc._2, Some(l), None)
              }
          }
        }

      r._1 match {
        case 1 =>
          State(r._4.get, r._3.get)
        case 2 =>
          State(r._2.get, r._4.get)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val state = State.parse(Source.fromResource("input-24.data").getLines())

    println(state)

    val endState = game(state)

    println(endState)

    println(s"solution 1: ${endState.units}")
  }
}
