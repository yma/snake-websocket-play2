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

	def alive: Boolean = weight > 0

	def live(tick: Int): Entity = new Entity(slot, weight-1, pos, updated)
	def explode(other: Entity, tick: Int) = new Entity(Slot.Item.exploded, (weight + other.weight) * 100, pos, tick)

	def tick(count: Int): Entity = if (weight > 1) live(count) else new Entity(slot, 0, pos, count)

	override def toString: String = "E"+ slot +"P"+ pos
}


class Mob(slot: Slot, weight: Int, pos: Position, val vector: Vector, val eaten: Boolean, updated: Int) extends Entity(slot, weight, pos, updated) {
	def respawn(weight: Int, pos: Position, vector: Vector, eaten: Boolean, tick: Int): Mob =
		new Mob(slot, weight, pos, vector, eaten, tick)

	def update(v: Vector): Mob = {
		if (v == vector || v == vector.reverse) this
		else respawn(weight, pos, v, eaten, updated)
	}

	def eat(food: Food) = respawn(weight + food.weight / 13, pos, vector, true, updated)

	def popTail(tick: Int) = if (eaten) {
		new Entity(Slot.Item.eatenFood, weight, pos, tick)
	} else {
		new Entity(slotCode, weight, pos, updated)
	}

	override def live(tick: Int): Entity = respawn(weight, pos + vector, vector, false, tick)

	override def toString: String = super.toString +"V"+ vector
}


class Food(weight: Int, pos: Position, updated: Int) extends Entity(Slot.Item.food, weight, pos, updated) {
	override def live(tick: Int): Entity = new Food(weight-1, pos, updated)
	def merge(other: Entity) = new Food(weight + other.weight, pos, updated)
}


class Area(val rand: Random, val width: Int, val height: Int, val entities: List[Entity], val updates: Map[Slot, Vector]) {
	def this(rand: Random, width: Int, height: Int) = this(rand, width, height, Nil, Map())
	def this(width: Int, height: Int) = this(new Random(), width, height)


	def randomPosition(): Position = new Position(rand.nextInt(width), rand.nextInt(height))
	def randomVector(): Vector = Vector.random(rand)

	def inside(pos: Position): Boolean = pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

	def update(slot: Slot, v: Vector): Area = new Area(rand, width, height, entities, updates + (slot -> v))

	def updatedEntities(tick: Int): List[Entity] = entities filter { _.updated == tick }

	def newMob(mob: Mob): Area = {
		assert(entities.filter(_.slot == mob.slot).isEmpty)
		new Area(rand, width, height, mob :: entities, updates)
	}


	def tick(count: Int, gameplay: Gameplay): Area = {
		var nextEntities = entities
		nextEntities = advance(count, nextEntities)
		nextEntities = gameplay.events(this, count, nextEntities)
		nextEntities = collision(count, nextEntities)
		new Area(rand, width, height, nextEntities, Map())
	}

	def advance(tick: Int, entities: List[Entity]): List[Entity] = {
		def snakeTails = for (e <- entities if e.isInstanceOf[Mob]) yield e.asInstanceOf[Mob].popTail(tick)
		def applyUpdate(entity: Entity) = entity match {
			case mob: Mob => updates.get(mob.slot) map { mob.update(_) } getOrElse mob
			case e => e
		}

		for {
			entity <- entities ::: snakeTails if entity.alive
			val nextTickEntity = applyUpdate(entity).tick(tick)
			if inside(nextTickEntity.pos)
		} yield nextTickEntity
	}

	def collision(tick: Int, entities: List[Entity]): List[Entity] = {
		def crash(entity: Entity, list: List[Entity]): Entity = entity match {
			case e:Mob => list match {
				case Nil => e
				case (mob:Mob) :: tail if mob.slot == e.slot => crash(e, tail)
				case (mob:Mob) :: tail => e.explode(mob, tick)
				case (food:Food) :: tail => crash(e.eat(food), tail)
				case other :: tail if other.alive => e.explode(other, tick)
				case other :: tail => crash(e, tail)
			}
			case e:Food => list match {
				case Nil => e
				case (food:Food) :: tail => crash(e.merge(food), tail)
				case other :: tail => crash(other, e :: tail)
			}
			case e => list match {
				case Nil => e
				case (mob:Mob) :: tail => crash(mob, e :: tail)
				case (food:Food) :: tail => crash(e, tail)
				case other :: tail if other.alive => e.explode(other, tick)
				case other :: tail => crash(e, tail)
			}
		}

		def reduce(group: List[Entity]): Entity = {
			group match {
				case Nil => throw new IllegalArgumentException()
				case entity :: tail => crash(entity, tail)
				case entity: Entity => entity
			}
		}
		entities.groupBy(_.pos).values.map(reduce(_)).toList
	}
}
