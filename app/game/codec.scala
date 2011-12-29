package game

package object codec {

	import gameplay.Mob
	import resource.Slot
	import server.Statistics

	object Codec {
		def encode(num: Int): Char = {
			assert(num >= 0)
			(num + 1).toChar
		}

		def decode(code: Char): Int = {
			val num = code.toInt
			assert(num > 0)
			num - 1
		}

		def encode[T](element: T)(implicit coder: Coder[T]): String = coder.encode(element)
		def encode[T](elements: Iterable[T])(implicit coder: Coder[T]): String = {
			elements.foldLeft("")(_ + encode(_))
		}

		def decode[T](code: String)(implicit coder: Coder[T]): T = coder.decode(code)
		def decodeList[T](code: String)(implicit coder: Coder[T]): List[T] = {
			code.grouped(coder.chunkSize).map(decode(_)).toList
		}

		trait Coder[T] {
			val chunkSize: Int
			def encode(element: T): String
			def decode(code: String): T
		}
	}

	implicit object SlotCoder extends Codec.Coder[Slot] {
		override val chunkSize: Int = 1
		override def encode(slot: Slot): String = Codec.encode(slot.value).toString
		override def decode(code: String): Slot = {
			assert(code.length == chunkSize)
			Slot(Codec.decode(code(0)))
		}
	}

	implicit object VectorCoder extends Codec.Coder[Vector] {
		override val chunkSize: Int = 1
		override def encode(vector: Vector): String = Codec.encode(vector.direction).toString
		override def decode(code: String): Vector = {
			assert(code.length == chunkSize)
			Vector.directions(Codec.decode(code(0)))
		}
	}

	implicit object PositionCoder extends Codec.Coder[Position] {
		override val chunkSize: Int = 2
		override def encode(pos: Position): String = ""+ Codec.encode(pos.x) + Codec.encode(pos.y)
		override def decode(code: String): Position = {
			new Position(Codec.decode(code(0)), Codec.decode(code(1)))
		}
	}

	implicit object EntityCoder extends Codec.Coder[Entity] {
		import resource._

		override val chunkSize: Int = 1 + PositionCoder.chunkSize

		override def encode(entity: Entity): String =
			Codec.encode((if (entity.alive) entity.slotCode else Slot.none).value) +
			PositionCoder.encode(entity.pos)

		override def decode(code: String): Entity = {
			assert(code.length == chunkSize)
			throw new RuntimeException()
		}
	}

	implicit object ScoreCoder extends Codec.Coder[Score] {
		import resource._

		override val chunkSize: Int = 3

		override def encode(score: Score): String =
			Codec.encode(Slot.Command.score) + Codec.encode(score.slot) + Codec.encode(score.value)

		override def decode(code: String): Score = {
			assert(code.length == chunkSize)
			throw new RuntimeException()
		}
	}

	implicit object ElementCoder extends Codec.Coder[Element] {
		import resource._

		override val chunkSize: Int = 0

		override def encode(element: Element): String = element match {
			case e: Entity => Codec.encode(e)
			case e: Score => Codec.encode(e)
		}

		override def decode(code: String): Score = throw new RuntimeException()
	}

	implicit object StatisticsCoder extends Codec.Coder[Statistics] {
		import resource._

		override val chunkSize: Int = 3

		override def encode(stats: Statistics): String =
			Codec.encode(Slot.Command.stats) + Codec.encode(stats.viewers) + Codec.encode(stats.players)

		override def decode(code: String): Statistics = {
			assert(code.length == chunkSize)
			throw new RuntimeException()
		}
	}

	object PlayerEnterCode {
		def apply(slot: Slot): String =
			Codec.encode(Slot.Command.playerEnter) + Codec.encode(slot)
	}

	object PlayerLeaveCode {
		def apply(slot: Slot): String =
			Codec.encode(Slot.Command.playerLeave) + Codec.encode(slot)
	}

	object PlayerDeadCode {
		def apply(slots: Iterable[Slot]): String =
			Codec.encode(Slot.Command.playerDead) + Codec.encode(slots)
	}

	object ResetNamesCode {
		def apply(): String =
			Codec.encode(Slot.Command.name) + Codec.encode(Slot.none)
	}

	object NameCode {
		def apply(slot: Slot, name: String): String =
			Codec.encode(Slot.Command.name) + Codec.encode(slot) + name
		def unapply(code: String): Option[String] =
			if (code.length < 1 || Slot(Codec.decode(code(0))) != Slot.Command.name) None
			else Some(code.substring(1))
	}

	object VectorCode {
		def apply(vector: Vector): Slot = Slot(vector.direction)
		def unapply(code: String): Option[Vector] = {
			val index = Slot.Command.Vector.index(Codec.decode[Slot](code))
			if (index < 4) Some(Vector.directions(index)) else None
		}
	}

}
