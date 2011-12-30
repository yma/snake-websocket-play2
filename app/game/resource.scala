package game.resource


case class Slot(value: Int)

object Slot {
	val none = Slot(0)

	class Range(val start: Int, val end: Int) {
		def this(range: Range, offset: Int) = this(range.start + offset, range.end + offset)
		lazy val slots = start.until(end).map(Slot(_)).toSet

		def contains(index: Int) = index >= 0 && index < end - start
		def contains(slot: Slot) = slot.value >= start && slot.value < end

		def slot(index: Int) = {
			assert(contains(index))
			Slot(start + index)
		}

		def index(slot: Slot) = {
			assert(contains(slot))
			slot.value - start
		}

		def from(range: Range, slot: Slot) = this.slot(range.index(slot))
	}

	class SubRange(superRange: Range, start: Int, end: Int)
	extends Range(superRange.start + start, superRange.start + end) {
		assert(superRange.contains(start) && superRange.contains(end))
	}

	object Players extends Range(1, 64)

	object Item extends Range(64, 96) {
		val food = slot(0)
		val eatenFood = slot(1)
		val exploded = slot(2)
		val evilSnake = slot(3)
	}

	object Command extends Range(96, 127) {
		val name = slot(0)
		val playerEnter = slot(1)
		val playerLeave = slot(2)
		val player = slot(4)
		val stats = slot(5)

		object Vector extends SubRange(Command, 6, 10) {
			val up = slot(0)
			val right = slot(1)
			val down = slot(2)
			val left = slot(3)
		}
	}

	object Gameplay extends Range(1000, 2000)

	object Mob extends Range(Players, 10000)
}
