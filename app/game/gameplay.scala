package game.gameplay

import game._
import game.resource._
import game.server._


abstract class Gameplay() {

	def tick(instance: Instance, area: Area, count: Int): Area = {
		var entities = area.entities
		entities = advance(area, count, entities)
		entities = events(area, count, entities)
		entities = collision(area, count, entities)
		area.nextTick(entities)
	}

	def leave(instance: Instance, area: Area, tick: Int, slot: Slot): Area = { area }

	def advance(area: Area, tick: Int, entities: List[Entity]): List[Entity] = entities
	def events(area: Area, tick: Int, entities: List[Entity]): List[Entity] = entities

	def collision(area: Area, tick: Int, entities: List[Entity]): List[Entity] = {
		def reduce(entity: Entity, list: List[Entity]): Entity = list match {
			case Nil => entity
			case other :: tail => {
				if (!entity.alive) reduce(other, tail)
				else if (!other.alive) reduce(entity, tail)
				else reduce(crash(area, tick, entity, other), tail)
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

	def crash(area: Area, tick: Int, entity: Entity, other: Entity): Entity {}

}
