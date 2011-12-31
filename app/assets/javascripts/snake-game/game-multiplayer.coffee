ctx = map = null
gridWidth = gridHeight = 0
gridDotSize = 4
players = {}
currentSlot = 0

KEY =
	LEFT_ARROW: 37
	UP_ARROW: 38
	RIGHT_ARROW: 39
	DOWN_ARROW: 40

Item =
	0: # players
		r: 127
		g: 127
		b: 127
	64: # food
		r: 0
		g: 127
		b: 0
	65: # eatenFood
		r: 0
		g: 192
		b: 0
	66: # exploded
		r: 255
		g: 0
		b: 0
	67: # evilSnake
		r: 192
		g: 0
		b: 0

rgb = (item) -> "rgb(" + item.r + "," + item.g + "," + item.b + ")"
rgba = (item, alpha) -> "rgba(" + item.r + "," + item.g + "," + item.b + ", " + alpha + ")"

gameCodecEncode = (num) ->
	if num < 0
		alert("Can't encode "+ num +" ("+ size +")")
	String.fromCharCode(num + 1)

gameCodecDecode = (code) ->
	if code.length != 1
		alert("Invalid code "+ code)
	code.charCodeAt(0) - 1

decodeEntity = (code) ->
	id: gameCodecDecode(code[0])
	pos:
		x: gameCodecDecode(code[1])
		y: gameCodecDecode(code[2])


drawBlock = (pos, inside, edges) ->
	eraseBlock pos
	ctx.strokeStyle = rgb(edges)
	ctx.strokeRect pos.x * gridDotSize + 0.5, pos.y * gridDotSize + 0.5, gridDotSize - 1, gridDotSize - 1
	if inside == edges
		ctx.fillStyle = rgba(inside, 0.5)
	else
		ctx.fillStyle = rgb(inside)
	ctx.fillRect pos.x * gridDotSize + 0.5, pos.y * gridDotSize + 0.5, gridDotSize - 1, gridDotSize - 1

eraseBlock = (pos) ->
	ctx.fillStyle = "#27005b"
	ctx.fillRect pos.x * gridDotSize, pos.y * gridDotSize, gridDotSize, gridDotSize

tick = (gameCode) ->
	while gameCode != ""
		entity = decodeEntity gameCode
		gameCode = gameCode.substring(3)
		if entity.id == 100 # player
			updatePlayer(entity.pos.x, entity.pos.y)
		else if entity.id != 0
			insideColor = edgesColor = Item[entity.id]
			if insideColor == undefined
				edgesColor = Item[0]
				player = players[entity.id]
				if player == undefined
					insideColor = edgesColor
				else
					insideColor = player.color
			drawBlock(entity.pos, insideColor, edgesColor)
		else
			eraseBlock entity.pos

resetPlayers = ->
	$("#players").html("")

newPlayer = (slot, color, name) ->
	block = """
		<div id="player-slot-${slot}" class="player">
			<div class="square" style="background-color: ${color};"></div>
			<span class="name">${name}</span>
			<span class="score">?</span>
		</div>
		"""
		.replace("${slot}", slot)
		.replace("${color}", rgb(color))
		.replace("${name}", name)
	$("#players").append(block)

	player = players[slot]
	if player == undefined
		player =
			slot: slot,
			status: 1,
			score: "?"
	else
		player.color = color
		players[slot] = player
	updatePlayerDisplay(player)

updatePlayer = (slot, statusScore) ->
	player = players[slot]
	if player == undefined
		player =
			slot: slot,
			color: Item[0]
	player.status = statusScore & 1
	player.score = (statusScore & ~1) / 2
	players[slot] = player
	updatePlayerDisplay(player)

updatePlayerDisplay = (player) ->
	$div = $("#player-slot-" + player.slot)
	if player.slot == currentSlot
		$div.addClass("me")
	else
		$div.removeClass("me")
	if player.status
		$div.addClass("dead")
	else
		$div.removeClass("dead")
	$(".score", $div).text(player.score)

removePlayer = (slot) ->
	$("#player-slot-" + slot).remove()
	delete players[slot]

updateStats = (viewers, players) ->
	$("#stats .viewers").text(viewers)
	$("#stats .players").text(players)

connectServer = (url, init, enter) ->
	ws = new WebSocket(url)
	ws.onopen = (e) ->
		$(".gameOver").hide()
	ws.onclose = (e) ->
		$(".gameOver .inner").text("La connexion au serveur a été fermé")
		$(".gameOver").show()
	ws.onmessage = (e) ->
		if e.data
			code = gameCodecDecode(e.data[0])
			if code == 96 # name
				slot = gameCodecDecode(e.data[1])
				if slot == 0
					resetPlayers()
				else
					color =
						r: gameCodecDecode(e.data[2]) * 2
						g: gameCodecDecode(e.data[3]) * 2
						b: gameCodecDecode(e.data[4]) * 2
					newPlayer(slot, color, e.data.substring(5))
			else if code == 97 # player enter
				currentSlot = gameCodecDecode(e.data[1])
				gridWidth = gameCodecDecode(e.data[2])
				gridHeight = gameCodecDecode(e.data[3])
				init()
				enter(currentSlot)
			else if code == 98 # player leave
				removePlayer(gameCodecDecode(e.data[1]))
			else if code == 101 # stats
				updateStats(gameCodecDecode(e.data[1]), gameCodecDecode(e.data[2]))
			else
				tick(e.data)
	ws.onerror = (e) -> alert("error: "+ e.data)

	spawn: (color, name) -> ws.send(
		gameCodecEncode(96) +
		gameCodecEncode(color.r / 2) +
		gameCodecEncode(color.g / 2) +
		gameCodecEncode(color.b / 2) +
		name)
	command: (vector) -> ws.send(gameCodecEncode(102 + vector))
	close: -> ws.close()

init = ->
	map = (0 for y in [0..(gridHeight - 1)] for x in [0..(gridWidth - 1)])

	canvas = document.getElementById "game"
	canvas.width = gridWidth * gridDotSize
	canvas.height = gridHeight * gridDotSize
	ctx = canvas.getContext "2d"
	$(".gameOver")
		.css("width", (canvas.width + 6) + "px")
		.css("height", (canvas.height + 6) + "px")

	ctx.fillStyle = "#27005b"
	ctx.fillRect 0, 0, canvas.width, canvas.height

$ ->
	$("#nameModal").hide()
	server = connectServer snakeGameWebsocketViewer, init, ->

	$(document).keydown (event) ->
		switch event.keyCode
			when KEY.LEFT_ARROW then server.command(3); break;
			when KEY.UP_ARROW then server.command(0); break;
			when KEY.RIGHT_ARROW then server.command(1); break;
			when KEY.DOWN_ARROW then server.command(2); break;

	$(".restart a").click ->
		init()
		$(".gameOver").hide()

	$("#play").submit ->
		server.close()
		server = connectServer snakeGameWebsocketPlayer, init, (slot) ->
			$("#nameModal").show()
			$("#nameModal form input[type=text]").val("Player" + slot).focus()
		return false

	$("#nameModal form").submit ->
		name = $("#nameModal form input[type=text]").val()
		$("#nameModal").hide()
		server.spawn(
			r: Math.random() * 126
			g: Math.random() * 126
			b: Math.random() * 126,
			name)
		return false
