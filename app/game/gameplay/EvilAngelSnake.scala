package game.gameplay

import scala.util.Random

import game._
import game.resource._
import game.server._


class EvilAngelSnake extends BasicSnake {
	override def tick(instance: Instance, area: Area, count: Int): Area = {
		if (area.rand.nextInt(50) == 0) new EvilClient(instance)
		super.tick(instance, area, count)
	}

	override def explode(entity: Entity, other: Entity, tick: Int) =
		new Entity(Slot.Item.exploded, (entity.weight + other.weight) * 100, entity.pos, tick)
}


class EvilMob(slot: Slot, weight: Int, pos: Position, vector: Vector, eaten: Boolean, updated: Int) extends Mob(slot, weight, pos, vector, eaten, updated) {
	override val slotCode: Slot = Slot.Item.evilSnake

	override def respawn(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tick: Int): Mob =
		new EvilMob(slot, weight, pos, vector, eaten, tick)

	override def live(tick: Int): Entity = respawn(weight, pos + vector, vector, false, tick)
}


class EvilClient(instance: Instance) extends Client(instance) {
	private val rand = new Random()
	override def slots: Slots = instance.gameplaySlots

	override def clientSlot(slot: Slot) { instance ! message.instance.Spawn(this) }

	override def dead(entity: Entity) { this ! 'stop }

	override def tick(count: Int, code: String) {
		if (rand.nextInt(3) == 0) {
			instance ! message.instance.Command(this, Vector.random(rand))
		}
	}

	override def spawnMob(slot: Slot, pos: Position, vector: Vector, tick: Int): Mob = {
		new EvilMob(slot, 10, pos, vector, false, tick)
	}
}
