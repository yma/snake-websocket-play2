package game

package object codec {

	import gameplay.Mob
	import resource.Slot
	import server.Statistics

	object Codec {
		def valid(num: Int) = num >= 0
		def encode(num: Int): Char = {
			assert(valid(num))
			(num + 1).toChar
		}

		def valid(code: Char) = code.toInt > 0
		def decode(code: Char): Int = {
			assert(valid(code))
			code.toInt - 1
		}

		def encode[T](element: T)(implicit coder: Encoder[T]): String = coder.encode(element).get
		def encode[T](elements: Iterable[T])(implicit coder: Encoder[T]): String = {
			elements.foldLeft("")(_ + encode(_))
		}

		def decode[T](code: String)(implicit coder: Decoder[T]): T = coder.decode(code).get._2
		def decodeList[T](code: String)(implicit coder: Decoder[T]): List[T] = {
			def extract(code: String, list: List[T]): List[T] = code match {
				case "" => list
				case coder(chunkSize, element) => extract(code.substring(chunkSize), element :: list)
			}
			extract(code, Nil).reverse
		}

		trait Encoder[T] {
			def encode(element: T): Option[String]
		}

		trait Decoder[T] {
			def decode(code: String): Option[(Int, T)]
			def unapply(code: String): Option[(Int, T)] = decode(code)
		}
	}

	implicit object SlotCoder extends Codec.Encoder[Slot] with Codec.Decoder[Slot] {
		override def encode(slot: Slot) =
			if (!Codec.valid(slot.value)) None
			else Some(Codec.encode(slot.value).toString)

		override def decode(code: String) =
			if (code.isEmpty || !Codec.valid(code(0))) None
			else Some(1, Slot(Codec.decode(code(0))))
	}

	implicit object VectorCoder extends Codec.Encoder[Vector] with Codec.Decoder[Vector] {
		override def encode(vector: Vector) =
			Some(Codec.encode(Slot.Command.Vector.slot(vector.direction)).toString)

		override def decode(code: String) = {
			val slot = SlotCoder.decode(code)
			if (slot == None || !Slot.Command.Vector.contains(slot.get._2)) None
			else Some(slot.get._1, Vector.directions(Slot.Command.Vector.index(slot.get._2)))
		}
	}

	implicit object PositionCoder extends Codec.Encoder[Position] with Codec.Decoder[Position] {
		override def encode(pos: Position) =
			if (!Codec.valid(pos.x) || !Codec.valid(pos.y)) None
			else Some(""+ Codec.encode(pos.x) + Codec.encode(pos.y))

		override def decode(code: String) =
			if (code.length < 2 || !Codec.valid(code(0)) || !Codec.valid(code(1))) None
			else Some(2, Position(Codec.decode(code(0)), Codec.decode(code(1))))
	}

	implicit object EntityCoder extends Codec.Encoder[Entity] {
		override def encode(entity: Entity) = {
			val slot = SlotCoder.encode(if (entity.alive) entity.slotCode else Slot.none)
			if (slot == None) None
			else {
				val pos = PositionCoder.encode(entity.pos)
				if (pos == None) None
				else Some(slot.get + pos.get)
			}
		}
	}

	implicit object PlayerCoder extends Codec.Encoder[Player] {
		override def encode(player: Player) = {
			val status = player.status match {
				case 'alive => Some(0)
				case 'dead => Some(1)
				case _ => None
			}
			val value = status.map(_ + player.score * 2)
			if (value == None || !Codec.valid(value.get)) None
			else {
				val slot = SlotCoder.encode(player.slot)
				if (slot == None) None
				else Some(Codec.encode(Slot.Command.player) + slot.get + Codec.encode(value.get))
			}
		}
	}

	implicit object ElementCoder extends Codec.Encoder[Element] {
		override def encode(element: Element) = element match {
			case e: Entity => EntityCoder.encode(e)
			case e: Player => PlayerCoder.encode(e)
		}
	}

	implicit object StatisticsCoder extends Codec.Encoder[Statistics] {
		override def encode(stats: Statistics) =
			if (!Codec.valid(stats.viewers) || !Codec.valid(stats.players)) None
			else Some(Codec.encode(Slot.Command.stats) + Codec.encode(stats.viewers) + Codec.encode(stats.players))
	}

	object PlayerEnterCode {
		def apply(slot: Slot, area: Area): String =
			Codec.encode(Slot.Command.playerEnter) +
			Codec.encode(slot) +
			Codec.encode(area.width) +
			Codec.encode(area.height)
	}

	object PlayerLeaveCode {
		def apply(slot: Slot): String =
			Codec.encode(Slot.Command.playerLeave) + Codec.encode(slot)
	}

	object ResetNamesCode {
		def apply(): String =
			Codec.encode(Slot.Command.name) + Codec.encode(Slot.none)
	}

	object NameCode {
		def apply(slot: Slot, name: String): String =
			Codec.encode(Slot.Command.name) + Codec.encode(slot) + name
	}

	object ClientNameCode {
		def unapply(code: String): Option[String] = {
			val slot = SlotCoder.decode(code)
			if (slot == None || slot.get._2 != Slot.Command.name) None
			else Some(code.substring(slot.get._1))
		}
	}

	object VectorCode {
		def apply(vector: Vector): String = Codec.encode(vector)

		def unapply(code: String): Option[Vector] = {
			val vector = VectorCoder.decode(code)
			if (vector == None) None else Some(vector.get._2)
		}
	}

}
