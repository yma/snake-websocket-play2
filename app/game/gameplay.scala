package game.gameplay

import game._
import game.resource._
import game.server._


abstract class Gameplay() {

	def elements: Seq[Element] = Nil
	def elementsAt(tickCount: Int): Seq[Element] = elements filter { _.tickCount == tickCount }

	def enter(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = { area }
	def death(instance: Instance, area: Area, tickCount: Int, entities: Seq[Entity]): Area = { area }
	def leave(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = { area }

	def tick(instance: Instance, area: Area, tickCount: Int): Area = {
		var entities = area.entities
		entities = advance(area, tickCount, entities)
		entities = events(area, tickCount, entities)
		entities = collision(area, tickCount, entities)
		area.nextTick(entities)
	}

	def advance(area: Area, tickCount: Int, entities: List[Entity]): List[Entity] = entities
	def events(area: Area, tickCount: Int, entities: List[Entity]): List[Entity] = entities

	def collision(area: Area, tickCount: Int, entities: List[Entity]): List[Entity] = {
		def reduce(entity: Entity, list: List[Entity]): Entity = list match {
			case Nil => entity
			case other :: tail => {
				if (!entity.alive) reduce(other, tail)
				else if (!other.alive) reduce(entity, tail)
				else reduce(crash(area, tickCount, entity, other), tail)
			}
		}

		def reduceGroup(group: List[Entity]): Entity = {
			group match {
				case Nil => throw new IllegalArgumentException()
				case entity :: tail => reduce(entity, tail)
				case entity: Entity => entity
			}
		}

		entities.groupBy(_.pos).values.map(reduceGroup(_)).toList
	}

	def crash(area: Area, tickCount: Int, entity: Entity, other: Entity): Entity

}
