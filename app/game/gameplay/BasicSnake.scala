package game.gameplay

import game._
import game.resource._
import game.server._


class BasicSnake extends BasicGameplay {
	override def events(area: Area, tickCount: Int, entities: List[Entity]): List[Entity] = {
		if (area.rand.nextInt(25) == 0) {
			new Food(20 + area.rand.nextInt(80), area.randomPosition(), tickCount) :: entities
		} else entities
	}

	override def crash(area: Area, tickCount: Int, entity: Entity, other: Entity): Entity = {
		(entity, other) match {
			case (e: Mob, o: Food) => eat(e, o)
			case (e: Food, o: Mob) => eat(o, e)
			case (e: Food, o: Food) => merge(e, o)
			case (e: Food, o) => o
			case (e, o: Food) => e
			case (e, o) => super.crash(area, tickCount, entity, other)
		}
	}

	def eat(mob: Mob, food: Food) = mob.copy2(weight = mob.weight + 1, eaten = true)

	def merge(food: Food, other: Food) =
		new Food(food.weight + other.weight, food.pos, math.min(food.tick, other.tick))
}


class Food(weight: Int, pos: Position, tick: Int) extends Entity(Slot.Item.food, weight, pos, tick) {
	override def copy(weight: Int, pos: Position, tick: Int) = new Food(weight, pos, tick)
	override def live(tickCount: Int): Entity = copy(weight = weight - 1)
}
