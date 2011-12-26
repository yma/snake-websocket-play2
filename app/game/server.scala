package game.server

import game._
import game.codec._
import game.gameplay._
import game.resource._
import java.util.Date
import play.api.libs.iteratee._
import scala.actors._
import scala.actors.Actor._
import scala.util.Random


package message {
	package instance {
		case class Command(client: Client, vector: Vector)
		case class ClientSlot(client: Client, slot: Slot)
		case class FullAreaCode(client: Client)
		case class Leave(client: Client)
		case class Spawn(client: Client)
		case class Tick(ticker: Ticker, count: Int)
	}

	package client {
		case class Command(code: String)
		case class ClientSlot(slot: Slot)
		case class Dead(entity: Entity)
		case class Tick(count: Int, code: String)
	}

	package slots {
		case class Register(instance: Instance, client: Client, area: Area, player: Boolean)
		case class Unregister(slot: Slot)
	}
}


class Instance(val name: String, private var area: Area, gameplay: Gameplay) extends Actor {
	val playerSlots = new Slots(Slot.Players)
	val gameplaySlots = new Slots(Slot.Gameplay)

	private var tickCount = 0
	private var ticker: Option[Ticker] = None

	private val clientSlots = scala.collection.mutable.Map[Client, Slot]()
	private val fullAreaCode = scala.collection.mutable.Map[Client, Boolean]()

	start()

	private def startTicker() {
		if (ticker == None) {
			play.api.Logger.debug("instance turn on")
			ticker = Option(new Ticker(100, tickCount))
			ticker.get.waitNextTick(this, tickCount)
		}
	}

	private def stopTicker() {
		if (ticker != None) {
			play.api.Logger.debug("instance turn off")
			ticker.get ! 'stop
			ticker = None
		}
	}

	def updateMob(mobSlot: Slot, vector: Vector) {
		area = area.update(mobSlot, vector)
	}

	def clientEnter(client: Client, player: Boolean) {
		client.slots ! message.slots.Register(this, client, area, player)
	}

	def clientLeave(client: Client) {
		this ! message.instance.Leave(client)
	}

	def act() {
		import message.instance._
		loop {
			react {
				case ClientSlot(client, slot) => {
					clientSlots += client -> slot
					fullAreaCode += client -> true
					startTicker()
					client ! message.client.ClientSlot(slot)
				}

				case Spawn(client) => {
					val slot = clientSlots(client)
					if (slot != Slot.none && area.entities.filter(_.slot == slot).isEmpty) {
						val mob = client.spawnMob(slot, area.randomPosition(), area.randomVector(), tickCount + 1)
						area = area.update(mob :: area.entities)
					}
				}

				case Leave(client) => {
					client.slots ! message.slots.Unregister(clientSlots(client))
					clientSlots -= client
					if (clientSlots.isEmpty) {
						stopTicker()
					}
				}

				case Command(client, vector) => {
					updateMob(clientSlots(client), vector)
				}

				case FullAreaCode(client) => {
					fullAreaCode += client -> true
				}
				case Tick(ticker, count) => {
					tickCount = count
					ticker.waitNextTick(this, count)

					val beforeEntities = area.entities

					area = gameplay.tick(this, area, count)

					val afterEntities = area.entities
					val clientSlotsSnapshot = clientSlots.toMap

					actor {
						val beforeMobs = (for (e <- beforeEntities if e.alive && e.isInstanceOf[Mob]) yield { e.slot -> e }).toMap
						val afterMobSlots = (for (e <- afterEntities if e.alive && e.isInstanceOf[Mob]) yield { e.slot }).toSet
						val deadMobSlots = (beforeMobs.keySet -- afterMobSlots).toSet

						for ((client, slot) <- clientSlotsSnapshot if deadMobSlots.contains(slot))
							client ! message.client.Dead(beforeMobs(slot))
					}

					lazy val entitiesCode = Codec.encode(area.entities)
					lazy val updatedEntitiesCode = Codec.encode(area.updatedEntities(count))
					for ((client, slot) <- clientSlots) {
						client ! message.client.Tick(count,
								if (fullAreaCode.getOrElse(client, false)) {
									fullAreaCode -= client
									entitiesCode
								} else updatedEntitiesCode)
					}
				}
			}
		}
	}
}


abstract class Client(instance: Instance, player: Boolean) extends Actor {
	def slots: Slots

	start()
	instance.clientEnter(this, player)

	def spawnMob(slot: Slot, pos: Position, vector: Vector, tick: Int): Mob

	def act() {
		import message.client._
		loop {
			react {
				case Command(code) => command(code)
				case ClientSlot(slot) => clientSlot(slot)
				case Dead(entity) => dead(entity)
				case Tick(count, code) => tick(count, code)
				case 'stop => { stop(); exit }
			}
		}
	}

	def command(code :String) {
		import message.instance._
		if (code.size == 1 && Codec.decode(code(0)) == 254) instance ! Spawn(this)
		else instance ! Command(this, Codec.decode[Vector](code))
	}

	def clientSlot(slot: Slot) {}

	def dead(entity: Entity) {}

	def stop() {
		instance.clientLeave(this)
	}

	def tick(count: Int, code: String) {}
}

class PlayerClient(instance: Instance, player: Boolean, out: Iteratee[String, Unit]) extends Client(instance, player) {
	override def slots: Slots = instance.playerSlots
	override def tick(count: Int, code: String) { out.feed(new Input.El(code)) }

	override def spawnMob(slot: Slot, pos: Position, vector: Vector, tick: Int): Mob = {
		new Mob(slot, 3, pos, vector, false, tick)
	}
}


class Slots(allocable: Slot.Range) extends Actor {
	private val registered = scala.collection.mutable.Set[Slot]()
	private val rand = new Random()

	start()

	private def allocSlot(entities: Seq[Slot]): Slot = {
		val freeSlots = (allocable.slots -- registered -- entities).toSeq
		if (freeSlots.isEmpty) Slot.none
		else {
			val slot = freeSlots(rand.nextInt(freeSlots.size))
			registered += slot
			slot
		}
	}

	def act() {
		import message.slots._
		loop {
			react {
				case Register(instance, client, area, player) => {
					instance ! message.instance.ClientSlot(client,
							if (player) allocSlot(area.entities map { _.slot }) else Slot.none)
				}
				case Unregister(slot) => {
					registered -= slot
				}
				case 'stop => exit
			}
		}
	}
}


class Ticker(val duration: Long, countOffset: Int) extends Actor {
	val startTime = System.currentTimeMillis() - countOffset * duration
	start()

	def waitNextTick(instance: Instance, count: Int) {
		this ! WaitTick(instance, count + 1)
	}

	case class WaitTick(instance: Instance, count: Int)

	private def sendTick(instance: Instance, count: Int) {
		instance ! message.instance.Tick(this, count)
	}

	def act() {
		loop {
			react {
				case WaitTick(instance, count) => {
					val countTime = startTime + count * duration
					val sleepTime = countTime - System.currentTimeMillis();
					if (sleepTime > 0) actor {
						self.reactWithin(sleepTime) {
							case TIMEOUT => sendTick(instance, count)
						}
					} else sendTick(instance, count)
				}
				case 'stop => exit
			}
		}
	}
}
