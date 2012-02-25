package controllers

import play.api._
import play.api.mvc._
import play.api.libs._
import play.api.libs.iteratee._
import play.api.libs.concurrent._

import game.gameplay._
import game.server.Instance

object MultiplayerSnakeGame extends Controller {

	def index = Action { implicit request =>
		Ok(views.html.index(request.headers("Host")))
	}

	val MainInstance = new Instance("main", new game.Area(40, 40), new EvilAngelSnake())

	def client(player: String) = WebSocket.async[String] { request =>
		import game.server.message.client._

		val client = new game.server.PlayerClient(MainInstance, player != "");

		val iteratee = Iteratee.foreach[String] { event =>
			client ! Command(event)
		}.mapDone { _ =>
			client ! 'stop
		}

		Promise.pure((iteratee, client.out))
	}

}
