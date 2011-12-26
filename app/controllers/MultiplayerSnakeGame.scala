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

	def client(player: String) = WebSocket[String] { request => (in, out) =>
		Logger.debug("connection")
		import game.server.message.client._
		val client = new game.server.PlayerClient(MainInstance, player != "", out);

		out <<: in.map {
			case Input.EOF => {
				Logger.info("disconnected")
				client ! 'stop
				Input.EOF
			}
			case Input.El(command) => {
				client ! Command(command)
				Input.Empty
			}
		}
	}

}
