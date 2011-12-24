package game.resource


object Slot {
	val none = 0

	trait Range {
		val slots: Set[Int]
	}

	object Players extends Range {
		override val slots = 1.until(128).toSet
	}
	object Item extends Range {
		override val slots = 128.until(160).toSet

		val food = 128
		val eatenFood = 129
		val exploded = 130
	}
}
