package game.gameplay

import scala.util.Random

import game._
import game.resource._
import game.server._


class BasicSnake extends BasicGameplay {
	override def events(area: Area, tick: Int, entities: List[Entity]): List[Entity] = {
		if (area.rand.nextInt(25) == 0) {
			new Food(20 + area.rand.nextInt(80), area.randomPosition(), tick) :: entities
		} else entities
	}

	override def crash(area: Area, tick: Int, entity: Entity, other: Entity): Entity = {
		(entity, other) match {
			case (e: Mob, o: Food) => eat(e, o)
			case (e: Food, o: Mob) => eat(o, e)
			case (e: Food, o: Food) => merge(e, o)
			case (e: Food, o) => o
			case (e, o: Food) => e
			case (e, o) => super.crash(area, tick, entity, other)
		}
	}

	def eat(mob: Mob, food: Food) = mob.respawn(mob.weight + 1, mob.pos, mob.vector, true, mob.updated)

	def merge(food: Food, other: Food) = new Food(food.weight + other.weight, food.pos, food.updated)
}


class Food(weight: Int, pos: Position, updated: Int) extends Entity(Slot.Item.food, weight, pos, updated) {
	override def live(tick: Int): Entity = new Food(weight-1, pos, updated)
}
