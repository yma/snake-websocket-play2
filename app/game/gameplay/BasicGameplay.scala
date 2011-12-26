package game.gameplay

import game._
import game.resource._
import game.server._


class BasicGameplay() extends Gameplay {
	override def advance(area: Area, tick: Int, entities: List[Entity]): List[Entity] = {
		def snakeTails = for (e <- entities if e.isInstanceOf[Mob]) yield e.asInstanceOf[Mob].popTail(tick)
		def applyUpdate(entity: Entity) = entity match {
			case mob: Mob => area.updates.get(mob.slot) map { mob.update(_) } getOrElse mob
			case e => e
		}

		for {
			entity <- entities ::: snakeTails if entity.alive
			val nextTickEntity = applyUpdate(entity).tick(tick)
			if area.inside(nextTickEntity.pos)
		} yield nextTickEntity
	}

	override def crash(area: Area, tick: Int, entity: Entity, other: Entity): (Symbol, Entity) = {
		(entity, other) match {
			case (e: Mob, o: Mob) if e.slot == o.slot => ('continue, e)
			case (e, o) => ('stop, explode(e, o, tick))
		}
	}

	def explode(entity: Entity, other: Entity, tick: Int) =
		new Entity(Slot.Item.exploded, math.max(entity.weight, other.weight), entity.pos, tick)
}


class Mob(slot: Slot, weight: Int, pos: Position, val vector: Vector, val eaten: Boolean, updated: Int) extends Entity(slot, weight, pos, updated) {
	def respawn(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tick: Int): Mob =
		new Mob(slot, weight, pos, vector, eaten, tick)

	def update(v: Vector): Mob = {
		if (v == vector || v == vector.reverse) this
		else respawn(weight, pos, v, eaten, updated)
	}

	def popTail(tick: Int) = if (eaten) {
		new Entity(Slot.Item.eatenFood, weight, pos, tick)
	} else {
		new Entity(slotCode, weight, pos, updated)
	}

	override def live(tick: Int): Entity = respawn(weight, pos + vector, vector, false, tick)

	override def toString: String = super.toString +"V"+ vector
}
