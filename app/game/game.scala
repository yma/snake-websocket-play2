package game

import scala.util.Random
import gameplay._
import resource._


case class Vector(direction: Int) {
	lazy val reverse: Vector = Vector.directions((direction + 2) & 0x3)

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


case class Position(x: Int, y: Int) {
	def +(v: Vector): Position = v match {
		case Vector.Up => new Position(x, y-1)
		case Vector.Right => new Position(x+1, y)
		case Vector.Down => new Position(x, y+1)
		case Vector.Left => new Position(x-1, y)
	}

	override def toString: String = x +","+ y
}


class Entity(val slot: Slot, val slotCode: Slot, val weight: Int, val pos: Position, val tick: Int) {
	def this(slot: Slot, weight: Int, pos: Position, tick: Int) = this(slot, slot, weight, pos, tick)
	val alive: Boolean = weight > 0

	def copy(
			weight: Int = this.weight,
			pos: Position = this.pos,
			tick: Int = this.tick) =
		new Entity(slot, weight, pos, tick)


	def dead(tickCount: Int): Entity =
		new Entity(slot, 0, pos, tickCount)

	def live(tickCount: Int): Entity =
		copy(weight = weight -1)

	def tick(tickCount: Int): Entity =
		if (weight > 1) live(tickCount) else dead(tickCount)

	override def toString: String = "E"+ slot +"P"+ pos
}


class Area(
		val rand: Random,
		val width: Int,
		val height: Int,
		val entities: List[Entity],
		val updates: Map[Slot, (Entity) => Entity]) {
	def this(rand: Random, width: Int, height: Int) = this(rand, width, height, Nil, Map())
	def this(width: Int, height: Int) = this(new Random(), width, height)

	def copy(
			rand: Random = this.rand,
			width: Int = this.width,
			height: Int = this.height,
			entities: List[Entity] = this.entities,
			updates: Map[Slot, (Entity) => Entity] = this.updates) =
		new Area(rand, width, height, entities, updates)


	def randomPosition(): Position = new Position(rand.nextInt(width), rand.nextInt(height))
	def randomVector(): Vector = Vector.random(rand)

	def inside(pos: Position): Boolean = pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

	def clip(pos: Position): Position = {
		val x = if (pos.x < 0) pos.x + width else if (pos.x >= width) pos.x - width else pos.x
		val y = if (pos.y < 0) pos.y + height else if (pos.y >= height) pos.y - height else pos.y
		Position(x, y)
	}

	def clip(entity: Entity, tickCount: Int): Entity = {
		val pos = clip(entity.pos)
		if (pos == entity.pos) entity
		else entity.copy(pos = pos, tick = tickCount)
	}

	def kill(list: Set[Entity], tickCount: Int): Area =
		copy(entities = entities map { e => if (list.contains(e)) e.dead(tickCount) else e })

	def updatedEntities(tickCount: Int): List[Entity] =
		entities filter { _.tick == tickCount }

	def nextTick(entities: List[Entity]): Area = {
		new Area(rand, width, height, entities, Map())
	}
}
