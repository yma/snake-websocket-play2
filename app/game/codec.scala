package game

package object codec {

	object Codec {
		def encode(num: Int): Char = {
			assert(num >= 0 && num < 255)
			(num + 1).toChar
		}

		def decode(code: Char): Int = {
			val num = code.toInt
			assert(num > 0 && num <= 255)
			num - 1
		}

		def encode[T](element: T)(implicit coder: Coder[T]): String = coder.encode(element)
		def encode[T](elements: List[T])(implicit coder: Coder[T]): String = {
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
		override val chunkSize: Int = 1 + PositionCoder.chunkSize
		override def encode(entity: Entity): String = Codec.encode(entity.id) + PositionCoder.encode(entity.pos)
		override def decode(code: String): Entity = {
			assert(code.length == chunkSize)
			new Entity(Codec.decode(code(0)), 0, PositionCoder.decode(code.substring(1)), 0)
		}
	}

}