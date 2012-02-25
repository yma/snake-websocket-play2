package game.gameplay

import scala.util.Random

import game._
import game.resource._
import game.server._


class EvilAngelSnake extends BasicSnake {
	override def tick(instance: Instance, area: Area, tickCount: Int): Area = {
		if (area.rand.nextInt(400) == 0) new EvilClient(instance)
		if (area.rand.nextInt(400) == 0) new AngelClient(instance)
		super.tick(instance, area, tickCount)
	}

	override def explode(entity: Entity, other: Entity, tickCount: Int) =
		new Entity(Slot.Item.exploded, (entity.weight + other.weight) * 20, entity.pos, tickCount)
}

abstract class ClientAutopilot(instance: Instance, clientSlot: Slot) extends Client(instance) {
	private val rand = new Random()
	protected var internalSlot = Slot.none
	override def slots: Slots = instance.gameplaySlots

	instance.clientEnter(this, true)

	override def enter(slot: Slot, area: Area) {
		super.enter(slot, area)
		internalSlot = this.slot
		this.slot = clientSlot
		instance ! message.instance.Spawn(this)
	}

	override def dead(entity: Entity) {
		super.dead(entity)
		this ! 'stop
	}

	override def tick(tickCount: Int, code: String) {
		super.tick(tickCount, code)
		if (rand.nextInt(3) == 0) instance ! message.instance.UpdateEntity(internalSlot,
				_.asInstanceOf[Mob].update(Vector.random(rand)))
	}
}


class EvilMob(slot: Slot, slotCode: Slot, weight: Int, pos: Position, vector: Vector, eaten: Boolean, updated: Int)
extends Mob(slot, slotCode, weight, pos, vector, eaten, updated) {
	override def copy2(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tickCount: Int) =
		new EvilMob(slot, slotCode, weight, pos, vector, eaten, tickCount)
}

class EvilClient(instance: Instance) extends ClientAutopilot(instance, Slot.Item.evilSnake) {
	override def spawnMob(pos: Position, vector: Vector, tickCount: Int): Mob =
		new EvilMob(internalSlot, slot, 10, pos, vector, false, tickCount)
}


class AngelMob(slot: Slot, slotCode: Slot, weight: Int, pos: Position, vector: Vector, eaten: Boolean, updated: Int)
extends Mob(slot, slotCode, weight, pos, vector, eaten, updated) {
	override def copy2(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tickCount: Int) =
		new AngelMob(slot, slotCode, weight, pos, vector, eaten, tickCount)

	override def popTail(tickCount: Int): Entity =
		if (eaten) super.popTail(tickCount) else new Food(weight, pos, tickCount)
}

class AngelClient(instance: Instance) extends ClientAutopilot(instance, Slot.Item.eatenFood) {
	override def spawnMob(pos: Position, vector: Vector, tickCount: Int): Mob =
		new AngelMob(internalSlot, slot, 5, pos, vector, false, tickCount)
}
