package game.resource


case class Slot(value: Int)

object Slot {
	val none = Slot(0)
	val name = Slot(200)
	val playerSlot = Slot(201)
	val score = Slot(202)
	val stats = Slot(203)

	class Range(start: Int, end: Int) {
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

	object Players extends Range(1, 128)

	object Item extends Range(128, 160) {
		val food = Slot(128)
		val eatenFood = Slot(129)
		val exploded = Slot(130)
		val evilSnake = Slot(131)
	}

	object Gameplay extends Range(1000, 2000)

	object Mob extends Range(10000, 10128)
}
