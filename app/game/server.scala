package game.server

import game._
import game.codec._
import java.util.Date
import play.api.libs.iteratee._
import scala.actors.Actor
import scala.util.Random


package message {
	package instance {
		case class Command(client: Client, vector: Vector)
		case class Enter(client: Client)
		case class FullAreaCode(client: Client)
		case class Leave(client: Client)
		case class PlayerId(client: Client, slot: Int)
		case class Tick(ticker: Ticker, count: Int)
	}

	package client {
		case class Command(code: String)
		case class PlayerId(slot: Int)
		case class Stop()
		case class Tick(code: String)
	}

	package slots {
		case class Stop()
		case class Register(instance: Instance, client: Client, area: Area)
		case class Unregister(slot: Int)
	}

	package ticker {
		case class Stop()
		case class WaitTick(instance: Instance, count: Int)
	}
}


class Instance(val name: String, private var area: Area) extends Actor {
	val players = new Slots(Slot.Players)

	private var tickCount = 0
	private var ticker: Option[Ticker] = None

	private val clientIds = scala.collection.mutable.Map[Client, Int]()
	private val fullAreaCode = scala.collection.mutable.Map[Client, Boolean]()

	start()

	private def startTicker() {
		if (ticker == None) {
			tickCount = 0
			ticker = Option(new Ticker(100))
			ticker.get ! message.ticker.WaitTick(this, tickCount)
		}
	}

	private def stopTicker() {
		if (ticker != None) {
			ticker.get ! message.ticker.Stop()
			ticker = None
		}
	}

	def act() {
		import message.instance._
		loop {
			react {
				case Enter(client) => {
					players ! message.slots.Register(this, client, area)
				}
				case PlayerId(client, id) => {
					if (id != Slot.none) area = area.newMob(id)
					clientIds += client -> id
					fullAreaCode += client -> true
					startTicker()
					client ! message.client.PlayerId(id)
				}

				case Leave(client) => {
					players ! message.slots.Unregister(clientIds(client))
					clientIds -= client
					if (clientIds.isEmpty) {
						stopTicker()
					}
				}

				case Command(client, vector) => {
					area = area.update(clientIds(client), vector)
				}

				case FullAreaCode(client) => {
					fullAreaCode += client -> true
				}
				case Tick(ticker, count) => {
					tickCount = count
					ticker ! message.ticker.WaitTick(this, count + 1)

					area = area.tick()

					lazy val entitiesCode = Codec.encode(area.entities)
					lazy val updatedEntitiesCode = Codec.encode(area.updatedEntities())
					for (client <- clientIds.keys) {
						client ! message.client.Tick(
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


class Client(instance: Instance, out: Iteratee[String, Unit]) extends Actor {
	start()
	instance ! message.instance.Enter(this)

	def act() {
		import message.client._
		loop {
			react {
				case Command(code) => {
					instance ! message.instance.Command(this, Codec.decode[Vector](code))
				}
				case PlayerId(id: Int) => {
				}
				case Stop() => {
					instance ! message.instance.Leave(this)
					exit()
				}
				case Tick(code) => {
					out.feed(new Input.El(code))
				}
			}
		}
	}
}


object Slot {
	val none = 0

	trait Range {
		val slots: Set[Int]
	}

	object Players extends Range {
		override val slots = 1.until(128).toSet
	}
	object Items extends Range {
		override val slots = 128.until(160).toSet
	}
}

class Slots(allocable: Slot.Range) extends Actor {
	private val registered = scala.collection.mutable.Set[Int]()
	private val rand = new Random()

	start()

	def allocSlot(entities: Seq[Int]): Int = {
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
				case Stop() => {
					exit()
				}
				case Register(instance, client, area) => {
					instance ! message.instance.PlayerId(client, allocSlot(area.entities map { _.id }))
				}
				case Unregister(slot) => {
					registered -= slot
				}
			}
		}
	}
}


class Ticker(val duration: Long) extends Actor {
	val startTime = System.currentTimeMillis()
	start()

	def act() {
		import message.ticker._
		loop {
			react {
				case Stop() => {
					exit()
				}
				case WaitTick(instance, count) => {
					val countTime = startTime + count * duration
					val sleepTime = countTime - System.currentTimeMillis();
					if (sleepTime > 0) Thread.sleep(sleepTime)
					instance ! message.instance.Tick(this, count)
				}
			}
		}
	}
}
