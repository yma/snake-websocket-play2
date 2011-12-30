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
		case class Enter(client: Client, slot: Slot)
		case class Leave(client: Client)
		case class FullAreaCode(client: Client)
		case class Name(client: Client, name: String)
		case class Spawn(client: Client)
		case class Tick(ticker: Ticker, count: Int)
		case class UpdateEntity(slot: Slot, f: (Entity) => Entity)
	}

	package client {
		case class Command(code: String)

		case class Enter(slot: Slot)
		case class Dead(entity: Entity)

		case class NotifyDead(slots: Iterable[Slot])
		case class NotifyLeave(slot: Slot)
		case class NotifyName(slot: Slot, name: String)
		case class NotifyNames(names: Map[Slot, String])
		case class NotifyStats(stats: Statistics)

		case class Tick(count: Int, code: String)
	}

	package slots {
		case class Register(instance: Instance, client: Client, area: Area, player: Boolean)
		case class Unregister(slot: Slot)
	}
}


class Statistics(val viewers: Int, val players: Int) {
	def update(action: Symbol, slot: Slot) = {
		if (slot == Slot.none || Slot.Players.contains(slot)) {
			val value = action match {
				case 'enter => 1
				case 'leave => -1
			}
			if (slot == Slot.none) new Statistics(viewers + value, players)
			else new Statistics(viewers, players + value)
		} else this
	}
}


class Instance(val name: String, private var area: Area, gameplay: Gameplay) extends Actor {
	val playerSlots = new Slots(Slot.Players)
	val gameplaySlots = new Slots(Slot.Gameplay)

	private var tickCount = 0
	private var ticker: Option[Ticker] = None

	private val clientSlots = scala.collection.mutable.Map[Client, Slot]()
	private val clientNames = scala.collection.mutable.Map[Client, String]()
	private val fullAreaCode = scala.collection.mutable.Map[Client, Boolean]()

	private var stats: Statistics = new Statistics(0, 0)

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

	private def notifyClients(message: Any) {
		for (client <- clientSlots.keys) client ! message
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
				case Enter(client, slot) => {
					clientSlots += client -> slot
					fullAreaCode += client -> true
					area = gameplay.enter(this, area, tickCount + 1, slot)
					stats = stats.update('enter, slot)
					startTicker()
					client ! message.client.Enter(slot)
					notifyClients(message.client.NotifyStats(stats))
				}

				case Name(client, name) => {
					clientNames += client -> name
					notifyClients(message.client.NotifyName(clientSlots(client), name))
				}

				case Spawn(client) => {
					val slot = clientSlots(client)
					if (slot != Slot.none && area.entities.filter(_.slot == slot).isEmpty) {
						val mob = client.spawnMob(area.randomPosition(), area.randomVector(), tickCount + 1)
						area = area.copy(entities = mob :: area.entities)
					}
				}

				case Leave(client) => {
					val slot = clientSlots(client)
					area = gameplay.leave(this, area, tickCount + 1, slot)
					stats = stats.update('leave, slot)

					notifyClients(message.client.NotifyLeave(slot))
					notifyClients(message.client.NotifyStats(stats))
					client.slots ! message.slots.Unregister(slot)
					clientSlots -= client
					clientNames -= client
					if (clientSlots.isEmpty) stopTicker()
				}

				case UpdateEntity(slot, f) => {
					area = area.copy(updates = area.updates + (slot -> f))
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

						notifyClients(message.client.NotifyDead(deadMobSlots))
						for ((client, slot) <- clientSlotsSnapshot if deadMobSlots.contains(slot))
							client ! message.client.Dead(beforeMobs(slot))
					}

					lazy val entitiesCode = Codec.encode(area.entities ++ gameplay.elements)
					lazy val updatedEntitiesCode = Codec.encode(area.entitiesAt(count) ++ gameplay.elementsAt(count))
					lazy val namesSnapshot = clientNames.map { case (c, n) => clientSlots(c) -> n }.toMap
					for ((client, slot) <- clientSlots) {
						if (fullAreaCode.getOrElse(client, false)) {
							client ! message.client.Tick(count, entitiesCode)
							client ! message.client.NotifyNames(namesSnapshot)
							fullAreaCode -= client
						} else {
							client ! message.client.Tick(count, updatedEntitiesCode)
						}
					}
				}
			}
		}
	}
}


abstract class Client(instance: Instance, player: Boolean) extends Actor {
	protected var slot: Slot = Slot.none
	def slots: Slots

	start()
	instance.clientEnter(this, player)

	def spawnMob(pos: Position, vector: Vector, tickCount: Int): Mob

	def act() {
		import message.client._
		loop {
			react {
				case Command(code) => command(code)
				case Enter(slot) => enter(slot)
				case Dead(entity) => dead(entity)

				case NotifyDead(slots) => notifyDead(slots)
				case NotifyLeave(slots) => notifyLeave(slots)
				case NotifyName(slot, name) => notifyName(slot, name)
				case NotifyNames(names) => notifyNames(names)
				case NotifyStats(stats) => notifyStats(stats)

				case Tick(count, code) => tick(count, code)
				case 'stop => { stop(); exit }
			}
		}
	}

	def command(code :String) {}

	def enter(slot: Slot) { this.slot = slot }
	def dead(entity: Entity) {}

	def notifyDead(slots: Iterable[Slot]) {}
	def notifyLeave(slot: Slot) {}
	def notifyName(slot: Slot, name: String) {}
	def notifyNames(names: Map[Slot, String]) {}
	def notifyStats(stats: Statistics) {}

	def tick(tickCount: Int, code: String) {}

	def stop() {
		instance.clientLeave(this)
	}
}

class PlayerClient(instance: Instance, player: Boolean, out: Iteratee[String, Unit])
extends Client(instance, player) {
	protected var mobSlot: Slot = Slot.none
	override def slots: Slots = instance.playerSlots

	private def send(code: String) { out.feed(new Input.El(code)) }

	override def spawnMob(pos: Position, vector: Vector, tickCount: Int): Mob = {
		new Mob(mobSlot, slot, 3, pos, vector, false, tickCount)
	}

	override def command(code :String) {
		super.command(code)
		import message.instance._
		code match {
			case VectorCode(vector) =>
				instance ! UpdateEntity(mobSlot, _.asInstanceOf[Mob].update(vector))
			case ClientNameCode(name) => {
				instance ! Name(this, name)
				instance ! Spawn(this)
			}
		}
	}

	override def enter(slot: Slot) {
		super.enter(slot)
		if (slot != Slot.none) {
			mobSlot = Slot.Mob.from(Slot.Players, slot)
			send(PlayerEnterCode(slot))
		} else mobSlot = Slot.none
	}

	override def notifyDead(slots: Iterable[Slot]) {
		super.notifyDead(slots)
		send(PlayerDeadCode(slots.filter(Slot.Players.contains)))
	}

	override def notifyLeave(slot: Slot) {
		super.notifyLeave(slot)
		if (Slot.Players contains slot) send(PlayerLeaveCode(slot))
	}

	private val notifyNamesDelayer = actor {
		loop {
			react {
				case ('name, slot: Slot, name: String) => {
					this ! message.client.NotifyName(slot, name)
					Thread.sleep(100)
				}
				case 'stop => exit
			}
		}
	}

	override def notifyNames(names: Map[Slot, String]) {
		super.notifyNames(names)
		send(ResetNamesCode())
		for ((slot, name) <- names) notifyNamesDelayer ! ('name, slot, name)
	}

	override def notifyName(slot: Slot, name: String) {
		super.notifyName(slot, name)
		send(NameCode(slot, name))
	}

	override def notifyStats(stats: Statistics) {
		super.notifyStats(stats)
		send(Codec.encode(stats))
	}

	override def tick(tickCount: Int, code: String) {
		super.tick(tickCount, code)
		send(code)
	}

	override def stop() {
		super.stop()
		notifyNamesDelayer ! 'stop
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
					instance ! message.instance.Enter(client,
							if (player) allocSlot(area.entities.map(_.slot)) else Slot.none)
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
