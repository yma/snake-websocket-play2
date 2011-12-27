package game

import scala.util.Random
import gameplay._
import resource._


class Vector(val direction: Int) {
	def reverse: Vector = Vector.directions((direction + 2) & 0x3)

	override def toString: String = direction.toString
}

object Vector {
	object Up extends Vector(0)
	object Right extends Vector(1)
	object Down extends Vector(2)
	object Left extends Vector(3)

	val directions = Map(
			Up.direction -> Up,
			Right.direction -> Right,
			Down.direction -> Down,
			Left.direction -> Left)

	def random(rand: Random): Vector = Vector.directions(rand.nextInt(4))
}


case class Position(val x: Int, val y: Int) {
	def +(v: Vector): Position = v match {
		case Vector.Up => new Position(x, y-1)
		case Vector.Right => new Position(x+1, y)
		case Vector.Down => new Position(x, y+1)
		case Vector.Left => new Position(x-1, y)
	}

	override def toString: String = x +","+ y
}


class Entity(val slot: Slot, val weight: Int, val pos: Position, val updated: Int) {
	val slotCode: Slot = slot

	def respawn(weight: Int, pos: Position, tick: Int): Entity =
		new Entity(slot, weight, pos, tick)

	def dead(tick: Int): Entity = new Entity(Slot.none, 1, pos, tick)

	def alive: Boolean = weight > 0

	def live(tick: Int): Entity = new Entity(slot, weight-1, pos, updated)

	def tick(count: Int): Entity = if (weight > 1) live(count) else new Entity(slot, 0, pos, count)

	override def toString: String = "E"+ slot +"P"+ pos
}


class Area(val rand: Random, val width: Int, val height: Int, val entities: List[Entity], val updates: Map[Slot, Vector]) {
	def this(rand: Random, width: Int, height: Int) = this(rand, width, height, Nil, Map())
	def this(width: Int, height: Int) = this(new Random(), width, height)


	def randomPosition(): Position = new Position(rand.nextInt(width), rand.nextInt(height))
	def randomVector(): Vector = Vector.random(rand)

	def inside(pos: Position): Boolean = pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

	def clip(pos: Position): Position = {
		val x = if (pos.x < 0) pos.x + width else if (pos.x >= width) pos.x - width else pos.x
		val y = if (pos.y < 0) pos.y + height else if (pos.y >= height) pos.y - height else pos.y
		Position(x, y)
	}

	def clip(entity: Entity, tick: Int): Entity = {
		val pos = clip(entity.pos)
		if (pos == entity.pos) entity
		else entity.respawn(entity.weight, pos, tick)
	}

	def kill(list: Set[Entity], tick: Int): Area = {
		update(entities map { e => if (list.contains(e)) e.dead(tick) else e })
	}

	def updatedEntities(tick: Int): List[Entity] = entities filter { _.updated == tick }

	def update(slot: Slot, v: Vector): Area = new Area(rand, width, height, entities, updates + (slot -> v))

	def update(entities: List[Entity]): Area = {
		new Area(rand, width, height, entities, updates)
	}

	def nextTick(entities: List[Entity]): Area = {
		new Area(rand, width, height, entities, Map())
	}
}
