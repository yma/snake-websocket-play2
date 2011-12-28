package game.gameplay

import game._
import game.resource._
import game.server._


class BasicGameplay() extends Gameplay {
	override def leave(instance: Instance, area: Area, tickCount: Int, slot: Slot): Area = {
		area.kill(area.entities.filter { e => e.slot == slot && e.isInstanceOf[Mob] }.toSet, tickCount)
	}

	override def advance(area: Area, tickCount: Int, entities: List[Entity]): List[Entity] = {
		def snakeTails = for (e <- entities if e.isInstanceOf[Mob]) yield e.asInstanceOf[Mob].popTail(tickCount)
		def applyUpdate(entity: Entity) = {
			area.updates.get(entity.slot).map(_(entity)).getOrElse(entity)
		}

		for (entity <- entities ::: snakeTails if entity.alive || entity.tick == tickCount)
			yield area.clip(applyUpdate(entity).tick(tickCount), tickCount)
	}

	override def crash(area: Area, tickCount: Int, entity: Entity, other: Entity): Entity = {
		(entity, other) match {
			case (e: Mob, o: Mob) if e.slot == o.slot => e
			case (e, o) => explode(e, o, tickCount)
		}
	}

	def explode(entity: Entity, other: Entity, tickCount: Int) =
		new Entity(Slot.Item.exploded, math.max(entity.weight, other.weight), entity.pos, tickCount)
}


class Mob(slot: Slot, slotCode: Slot, weight: Int, pos: Position, val vector: Vector, val eaten: Boolean, tick: Int)
extends Entity(slot, slotCode, weight, pos, tick) {
	override def copy(weight: Int, pos: Position, tick: Int) = copy2(weight, pos, vector, eaten, tick)

	def copy2(
			weight: Int = this.weight,
			pos: Position = this.pos,
			vector: Vector = this.vector,
			eaten: Boolean = this.eaten,
			tick: Int = this.tick) =
		new Mob(slot, slotCode, weight, pos, vector, eaten, tick)

	def update(v: Vector): Mob = {
		if (v == vector || v == vector.reverse) this
		else copy2(vector = v)
	}

	def popTail(tickCount: Int): Entity = if (eaten) {
		new Entity(Slot.Item.eatenFood, weight, pos, tickCount)
	} else {
		new Entity(slotCode, weight, pos, tick)
	}

	override def live(tickCount: Int): Entity =
		copy2(pos = pos + vector, eaten = false, tick = tickCount)

	override def toString: String = super.toString +"V"+ vector
}
