package game.gameplay

import game._
import game.server._


abstract class Gameplay() {
	def tick(instance: Instance, count: Int)
	def events(area: Area, tick: Int, entities: List[Entity]): List[Entity]
}
