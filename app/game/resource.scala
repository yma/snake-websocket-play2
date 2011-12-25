package game.resource


case class Slot(value: Int)

object Slot {
	val none = Slot(0)

	class Range(start: Int, end: Int) {
		val slots = start.until(end).map(Slot(_)).toSet
	}

	object Players extends Range(1, 128)

	object Item extends Range(128, 160) {
		val food = Slot(128)
		val eatenFood = Slot(129)
		val exploded = Slot(130)
		val evilSnake = Slot(131)
	}

	object Gameplay extends Range(1000, 2000)
}
