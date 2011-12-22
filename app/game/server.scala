package game.server

import game._
import java.util.Date
import play.api.libs.iteratee._
import scala.actors.Actor


package message {
	package instance {
		case class Command(client: Client, vector: Vector)
		case class Enter(client: Client)
		case class FullAreaCode(client: Client)
		case class Leave(client: Client)
		case class Tick(ticker: Ticker, count: Int)
	}

	package client {
		case class Command(code: String)
		case class Stop()
		case class Tick(code: String)
	}

	package ticker {
		case class Stop()
		case class Tick(instance: Instance, count: Int)
	}
}


class Instance(val name: String, private var area: Area) extends Actor {
	private var lastId: Int = 0
	private var tickCount = 0
	private var ticker: Option[Ticker] = None

	private val clientIds = scala.collection.mutable.Map[Client, Int]()
	private val fullAreaCode = scala.collection.mutable.Map[Client, Boolean]()

	start()

	private def startTicker() {
		if (ticker == None) {
			tickCount = 0
			ticker = Option(new Ticker(100))
			ticker.get ! message.ticker.Tick(this, tickCount)
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
				case Command(actor, vector) => {
					area = area.update(clientIds(actor), vector)
				}
				case Enter(client) => {
					lastId += 1
					clientIds += client -> lastId
					fullAreaCode += client -> true
					area = area.newMob(lastId, 30)
					startTicker()
				}
				case FullAreaCode(client) => {
					fullAreaCode += client -> true
				}
				case Leave(client) => {
					clientIds -= client
					if (clientIds.isEmpty) {
						stopTicker()
					}
				}
				case Tick(ticker, count) => {
					tickCount = count
					ticker ! message.ticker.Tick(this, count + 1)

					area = area.tick()

					lazy val entitiesCode = toCode(area.entities)
					lazy val updatedEntitiesCode = toCode(area.updatedEntities())
					def toCode(entities: List[Entity]) = entities.foldLeft("")(_ + _.elementCode)
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
					assert(code.length == 1)
					instance ! message.instance.Command(this, Vector.fromCode(code(0)))
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
				case Tick(actor: Actor, count: Int) => {
					val now = System.currentTimeMillis()
					val countTime = startTime + count * duration
					val sleepTime = countTime - now;
					if (sleepTime > 0) Thread.sleep(sleepTime)
					actor ! message.instance.Tick(this, count)
				}
			}
		}
	}
}
