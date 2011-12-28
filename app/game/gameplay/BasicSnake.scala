package game.gameplay

import game._
import game.resource._
import game.server._


class BasicSnake extends BasicGameplay {
	val scores = scala.collection.mutable.Map[Slot, Score]()

	override def elements: Seq[Element] = super.elements ++ scores.values

	override def enter(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = {
		scores += slot -> new Score(slot, 0, tickCount)
		super.enter(instance, area, tickCount, slot)
	}

	override def leave(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = {
		scores -= slot
		super.leave(instance, area, tickCount, slot)
	}

	override def events(area: Area, tickCount: Int, entities: List[Entity]): List[Entity] = {
		if (area.rand.nextInt(25) == 0) {
			new Food(20 + area.rand.nextInt(80), area.randomPosition(), tickCount) :: entities
		} else entities
	}

	override def crash(area: Area, tickCount: Int, entity: Entity, other: Entity): Entity = {
		(entity, other) match {
			case (e: Mob, o: Food) => eat(e, o, tickCount)
			case (e: Food, o: Mob) => eat(o, e, tickCount)
			case (e: Food, o: Food) => merge(e, o, tickCount)
			case (e: Food, o) => o
			case (e, o: Food) => e
			case (e, o) => super.crash(area, tickCount, entity, other)
		}
	}

	def eat(mob: Mob, food: Food, tickCount: Int): Mob = {
		scores.get(mob.slotCode) map { score =>
			scores(mob.slotCode) = score.add(1, tickCount)
		}
		mob.copy2(weight = mob.weight + 1, eaten = true)
	}

	def merge(food: Food, other: Food, tickCount: Int): Food =
		new Food(food.weight + other.weight, food.pos, math.min(food.tickCount, other.tickCount))
}


class Food(weight: Int, pos: Position, tickCount: Int) extends Entity(Slot.Item.food, weight, pos, tickCount) {
	override def copy(weight: Int, pos: Position, tickCount: Int) = new Food(weight, pos, tickCount)
	override def live(tickCount: Int): Entity = copy(weight = weight - 1)
}
