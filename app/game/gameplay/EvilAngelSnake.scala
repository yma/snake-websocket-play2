package game.gameplay

import scala.util.Random

import game._
import game.resource._
import game.server._


class EvilAngelSnake extends BasicSnake {
	override def tick(instance: Instance, area: Area, count: Int): Area = {
		if (area.rand.nextInt(400) == 0) new EvilClient(instance)
		if (area.rand.nextInt(400) == 0) new AngelClient(instance)
		super.tick(instance, area, count)
	}

	override def explode(entity: Entity, other: Entity, tick: Int) =
		new Entity(Slot.Item.exploded, (entity.weight + other.weight) * 20, entity.pos, tick)
}

abstract class ClientAutopilot(instance: Instance) extends Client(instance, true) {
	private val rand = new Random()
	override def slots: Slots = instance.gameplaySlots

	override def clientSlot(slot: Slot) { instance ! message.instance.Spawn(this) }

	override def dead(entity: Entity) { this ! 'stop }

	override def tick(count: Int, code: String) {
		if (rand.nextInt(3) == 0) {
			instance ! message.instance.ChangeVector(this, Vector.random(rand))
		}
	}
}


class EvilMob(slot: Slot, weight: Int, pos: Position, vector: Vector, eaten: Boolean, updated: Int) extends Mob(slot, weight, pos, vector, eaten, updated) {
	override val slotCode: Slot = Slot.Item.evilSnake

	override def respawn(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tick: Int): Mob =
		new EvilMob(slot, weight, pos, vector, eaten, tick)
}

class EvilClient(instance: Instance) extends ClientAutopilot(instance) {
	override def spawnMob(slot: Slot, pos: Position, vector: Vector, tick: Int): Mob = {
		new EvilMob(slot, 10, pos, vector, false, tick)
	}
}


class AngelMob(slot: Slot, weight: Int, pos: Position, vector: Vector, eaten: Boolean, updated: Int) extends Mob(slot, weight, pos, vector, eaten, updated) {
	override val slotCode: Slot = Slot.Item.eatenFood

	override def respawn(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tick: Int): Mob =
		new AngelMob(slot, weight, pos, vector, eaten, tick)

	override def popTail(tick: Int): Entity = if (eaten) super.popTail(tick) else new Food(weight, pos, tick)
}

class AngelClient(instance: Instance) extends ClientAutopilot(instance) {
	override def spawnMob(slot: Slot, pos: Position, vector: Vector, tick: Int): Mob = {
		new AngelMob(slot, 5, pos, vector, false, tick)
	}
}
