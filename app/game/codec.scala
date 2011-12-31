package game

package object codec {

	import gameplay.Mob
	import resource.Slot
	import server.Color
	import server.Name
	import server.Statistics

	object Codec {
		def rawEncode(num: Int): Option[Char] =
			if (num >= 0) Some((num + 1).toChar) else None

		def rawDecode(code: Char): Option[Int] =
			if (code.toInt > 0) Some(code.toInt - 1) else None

		def encode[T](element: T)(implicit coder: Encoder[T]): String = coder(element)
		def encode[T](elements: Iterable[T])(implicit coder: Encoder[T]): String = {
			elements.foldLeft("")(_ + encode(_))
		}

		def decode[T](code: String)(implicit coder: Decoder[T]): T = coder.decode(code).get._1
		def decodeList[T](code: String)(implicit coder: Decoder[T]): List[T] = {
			def extract(code: String, list: List[T]): List[T] = code match {
				case "" => list
				case coder(element, tail) => extract(tail, element :: list)
			}
			extract(code, Nil).reverse
		}

		trait Encoder[T] {
			def encode(element: T): String
			def apply(element: T): String = encode(element)
		}

		trait Decoder[T] {
			def decode(code: String): Option[(T, String)]
			def unapply(code: String): Option[(T, String)] = decode(code)
		}
	}

	implicit object IntCoder extends Codec.Encoder[Int] with Codec.Decoder[Int] {
		override def encode(value: Int) = {
			val code = Codec.rawEncode(value)
			assert(code != None)
			code.get.toString
		}

		override def decode(code: String) =
			if (code.isEmpty) None
			else {
				val num = Codec.rawDecode(code(0))
				if (num == None) None else Some(num.get, code.substring(1))
			}
	}

	implicit object SlotCoder extends Codec.Encoder[Slot] with Codec.Decoder[Slot] {
		override def encode(slot: Slot) = IntCoder(slot.value)

		override def decode(code: String) = code match {
			case IntCoder(num, tail) => Some(Slot(num), tail)
			case _ => None
		}
	}

	implicit object VectorCoder extends Codec.Encoder[Vector] with Codec.Decoder[Vector] {
		override def encode(vector: Vector) = SlotCoder(Slot.Command.Vector.slot(vector.direction))

		override def decode(code: String) = code match {
			case SlotCoder(slot, tail) if Slot.Command.Vector.contains(slot) =>
				Some(Vector.directions(Slot.Command.Vector.index(slot)), tail)
			case _ => None
		}
	}

	implicit object PositionCoder extends Codec.Encoder[Position] with Codec.Decoder[Position] {
		override def encode(pos: Position) = IntCoder(pos.x) + IntCoder(pos.y)

		override def decode(code: String) = code match {
			case IntCoder(x, IntCoder(y, tail)) => Some(Position(x, y), tail)
			case _ => None
		}
	}

	implicit object EntityCoder extends Codec.Encoder[Entity] {
		override def encode(entity: Entity) =
			SlotCoder(if (entity.alive) entity.slotCode else Slot.none) +
			PositionCoder(entity.pos)
	}

	implicit object PlayerCoder extends Codec.Encoder[Player] {
		override def encode(player: Player) =
			SlotCoder(Slot.Command.player) +
			SlotCoder(player.slot) +
			IntCoder((player.status match {
				case 'alive => 0
				case 'dead => 1
			}) + player.score * 2)
	}

	implicit object ElementCoder extends Codec.Encoder[Element] {
		override def encode(element: Element) = element match {
			case e: Entity => EntityCoder.encode(e)
			case e: Player => PlayerCoder.encode(e)
		}
	}

	implicit object StatisticsCoder extends Codec.Encoder[Statistics] {
		override def encode(stats: Statistics) =
			SlotCoder(Slot.Command.stats) + IntCoder(stats.viewers) + IntCoder(stats.players)
	}

	implicit object ColorCoder extends Codec.Encoder[Color] with Codec.Decoder[Color] {
		override def encode(color: Color) =
			IntCoder(color.red) +
			IntCoder(color.green) +
			IntCoder(color.blue)

		override def decode(code: String) = code match {
			case IntCoder(red, IntCoder(green, IntCoder(blue, tail))) => Some(Color(red, green, blue), tail)
			case _ => None
		}
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
		def apply(slot: Slot, name: Name): String =
			Codec.encode(Slot.Command.name) +
			Codec.encode(slot) +
			Codec.encode(name.color) +
			name.value
	}

	object ClientNameCode {
		def unapply(code: String): Option[Name] = code match {
			case SlotCoder(Slot.Command.name, ColorCoder(color, name)) => Some(Name(name, color))
			case _ => None
		}
	}

	object VectorCode {
		def apply(vector: Vector): String = Codec.encode(vector)

		def unapply(code: String): Option[Vector] = code match {
			case VectorCoder(vector, "") => Some(vector)
			case _ => None
		}
	}

}
