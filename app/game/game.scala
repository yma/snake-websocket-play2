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


class Entity(val id: Int, val weight: Int, val pos: Position, val updated: Int) {
	def alive: Boolean = weight > 0

	def live(tick: Int): Entity = new Entity(id, weight-1, pos, updated)
	def explode(other: Entity, tick: Int) = new Entity(resource.Slot.Item.exploded, weight + other.weight, pos, tick)

	def tick(count: Int): Entity = if (weight > 1) live(count) else new Entity(resource.Slot.none, 0, pos, count)

	override def toString: String = "E"+ id +"P"+ pos
}


class Mob(id: Int, weight: Int, pos: Position, val vector: Vector, val eaten: Boolean, updated: Int) extends Entity(id, weight, pos, updated) {
	def update(v: Vector): Entity = {
		if (v == vector || v == vector.reverse) this
		else new Mob(id, weight, pos, v, eaten, updated)
	}

	def eat(food: Food) = new Mob(id, weight + food.weight, pos, vector, true, updated)

	def popTail(tick: Int) = if (eaten) {
		new Entity(resource.Slot.Item.eatenFood, weight, pos, tick)
	} else {
		new Entity(id, weight, pos, updated)
	}

	override def live(tick: Int): Entity = new Mob(id, weight, pos + vector, vector, false, tick)

	override def toString: String = super.toString +"V"+ vector
}


class Food(weight: Int, pos: Position, updated: Int) extends Entity(resource.Slot.Item.food, weight, pos, updated) {
	override def live(tick: Int): Entity = new Food(weight-1, pos, updated)
	def merge(other: Entity) = new Food(weight + other.weight, pos, updated)
}


class Area(val rand: Random, val width: Int, val height: Int, val entities: List[Entity], val updates: Map[Int, Vector]) {
	def this(rand: Random, width: Int, height: Int) = this(rand, width, height, Nil, Map())
	def this(width: Int, height: Int) = this(new Random(), width, height, Nil, Map())


	def randomPosition(): Position = new Position(rand.nextInt(width), rand.nextInt(height))
	def randomVector(): Vector = Vector.directions(rand.nextInt(4))

	def inside(pos: Position): Boolean = pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

	def update(id: Int, v: Vector): Area = new Area(rand, width, height, entities, updates + (id -> v))

	def updatedEntities(tick: Int): List[Entity] = entities filter { _.updated == tick }

	def newMob(id: Int, tick: Int): Area = {
		assert(entities.filter(_.id == id).isEmpty)
		new Area(rand, width, height, new Mob(id, 3, randomPosition(), randomVector(), false, tick) :: entities, updates)
	}


	def tick(tick: Int): Area = {
		var nextEntities = entities
		nextEntities = advance(tick, nextEntities)
		nextEntities = gameplay(tick, nextEntities)
		nextEntities = collision(tick, nextEntities)
		new Area(rand, width, height, nextEntities, Map())
	}

	def advance(tick: Int, entities: List[Entity]): List[Entity] = {
		def snakeTails = for (e <- entities if e.isInstanceOf[Mob]) yield e.asInstanceOf[Mob].popTail(tick)
		def applyUpdate(entity: Entity) = entity match {
			case mob: Mob => updates.get(mob.id) map { mob.update(_) } getOrElse mob
			case e => e
		}

		for {
			entity <- entities ::: snakeTails if entity.alive
			val nextTickEntity = applyUpdate(entity).tick(tick)
			if inside(nextTickEntity.pos)
		} yield nextTickEntity
	}

	def gameplay(tick: Int, entities: List[Entity]): List[Entity] = {
		if (rand.nextInt(33) == 0) new Food(10 + rand.nextInt(40), randomPosition(), tick) :: entities
		else entities
	}

	def collision(tick: Int, entities: List[Entity]): List[Entity] = {
		def crash(entity: Entity, list: List[Entity]): Entity = entity match {
			case e:Mob => list match {
				case Nil => e
				case (mob:Mob) :: tail => if (mob.id == e.id) crash(e, tail) else e.explode(mob, tick)
				case (food:Food) :: tail => crash(e.eat(food), tail)
				case other :: tail => if (other.alive) e.explode(other, tick) else crash(e, tail)
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
				case other :: tail => if (other.alive) e.explode(other, tick) else crash(e, tail)
			}
		}

		def reduce(group: List[Entity]): Entity = {
			group match {
				case Nil => throw new IllegalArgumentException()
				case entity :: tail => crash(entity, tail)
				case entity: Entity => entity
			}
		}

		(for (group <- entities.groupBy(_.pos).values) yield reduce(group)).toList
	}
}
