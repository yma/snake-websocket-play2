package game

import scala.util.Random


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


class Entity(val id: Int, val weight: Int, val pos: Position, val updated: Boolean) {
	def alive: Boolean = id != 0

	def tick(): Entity = {
		if (weight > 0) new Entity(id, weight-1, pos, false)
		else new Entity(0, 0, pos, true)
	}

	override def toString: String = "E"+ id +"P"+ pos
}


class Mob(id: Int, weight: Int, pos: Position, val vector: Vector, updated: Boolean) extends Entity(id, weight, pos, updated) {
	def update(v: Vector): Entity = {
		if (v == vector || v == vector.reverse) this
		else new Mob(id, weight, pos, v, false)
	}

	override def tick(): Entity = new Mob(id, weight, pos + vector, vector, true)

	override def toString: String = super.toString +"V"+ vector
}


class Area(val rand: Random, val width: Int, val height: Int, val entities: List[Entity], val updates: Map[Int, Vector]) {
	def this(rand: Random, width: Int, height: Int) = this(rand, width, height, Nil, Map())
	def this(width: Int, height: Int) = this(new Random(), width, height, Nil, Map())

	def update(id: Int, v: Vector): Area = new Area(rand, width, height, entities, updates + (id -> v))

	def tick(): Area = {
		def snakeTails = {
			entities.filter(_ match {
				case _: Mob => true
				case _ => false
			}) map { m => new Entity(m.id, m.weight, m.pos, false) }
		}
		def applyUpdate(entity: Entity) = entity match {
			case mob: Mob => updates.get(mob.id) map { mob.update(_) } getOrElse mob
			case e => e
		}

		var nextTickEntities = for {
			entity <- entities ::: snakeTails if entity.alive
			val nextTickEntity = applyUpdate(entity).tick()
			if inside(nextTickEntity.pos)
		} yield nextTickEntity

		val crash = scala.collection.mutable.Map[Position, Int]()
		for (e <- nextTickEntities if e.alive) {
			val count = crash.getOrElse(e.pos, 0)
			crash += e.pos -> (count + 1)
		}
		nextTickEntities = nextTickEntities.filter({
			case e: Mob => crash.getOrElse(e.pos, 1) == 1
			case _ => true
		})

		new Area(rand, width, height, nextTickEntities, Map())
	}

	def randomPosition(): Position = new Position(rand.nextInt(width), rand.nextInt(height))
	def randomVector(): Vector = Vector.directions(rand.nextInt(4))

	def inside(pos: Position): Boolean = pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

	def updatedEntities(): List[Entity] = entities filter { _.updated }

	def newMob(id: Int): Area = {
		assert(entities.filter(_.id == id).isEmpty)
		new Area(rand, width, height, new Mob(id, 30, randomPosition(), randomVector(), true) :: entities, updates)
	}
}
