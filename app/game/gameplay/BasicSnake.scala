package game.gameplay

import game._
import game.resource._
import game.server._


class BasicSnake extends BasicGameplay {
	val players = scala.collection.mutable.Map[Slot, Player]()

	override def elements: Seq[Element] = super.elements ++ players.values

	override def enter(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = {
		if (Slot.Players contains slot) players += slot -> new Player(slot, 'alive, 0, tickCount)
		super.enter(instance, area, tickCount, slot)
	}

	override def death(instance: Instance, area: Area, tickCount: Int, entities: Seq[Entity]): Area = {
		for (entity <- entities) players.get(entity.slotCode) map { player =>
			players(entity.slotCode) = player.copy(status = 'dead, tickCount = tickCount)
		}
		super.death(instance, area, tickCount, entities)
	}

	override def leave(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = {
		players -= slot
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
		players.get(mob.slotCode) map { player =>
			players(mob.slotCode) = player.copy(score = player.score + 1, tickCount = tickCount)
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
