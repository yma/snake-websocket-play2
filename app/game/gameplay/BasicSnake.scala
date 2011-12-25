package game.gameplay

import scala.util.Random

import game._
import game.resource._
import game.server._


class BasicSnake extends Gameplay {
	private val rand = new Random()

	override def tick(instance: Instance, count: Int) {
		if (rand.nextInt(50) == 0) new EvilClient(instance)
	}

	override def events(area: Area, tick: Int, entities: List[Entity]): List[Entity] = {
		var nextEntities = entities
		if (area.rand.nextInt(33) == 0) {
			nextEntities = new Food(20 + area.rand.nextInt(80), area.randomPosition(), tick) :: entities
		}
		nextEntities
	}
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
