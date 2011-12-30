ctx = map = null
gridSize = 40
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


drawBlock = (pos, item) ->
	eraseBlock pos
	ctx.strokeStyle = rgb(item)
	ctx.strokeRect pos.x * 10 + 0.5, pos.y * 10 + 0.5, 8, 8
	ctx.fillStyle = rgba(item, 0.5)
	ctx.fillRect pos.x * 10 + 0.5, pos.y * 10 + 0.5, 8, 8

eraseBlock = (pos) ->
	ctx.fillStyle = "#27005b"
	ctx.fillRect pos.x * 10, pos.y * 10, 10, 10

tick = (gameCode) ->
	while gameCode != ""
		entity = decodeEntity gameCode
		gameCode = gameCode.substring(3)
		if entity.id == 100 # player
			updatePlayer(entity.pos.x, entity.pos.y)
		else if entity.id != 0
			item = Item[entity.id]
			if item == undefined
				item = Item[0]
			drawBlock(entity.pos, item)
		else
			eraseBlock entity.pos

resetPlayers = ->
	$("#players").html("")

newPlayer = (slot, name) ->
	block = """
		<div id="player-slot-${slot}" class="player">
			<div class="square" style="background-color: #e44;"></div>
			<span class="name">${name}</span>
			<span class="score">?</span>
		</div>
		"""
		.replace("${slot}", slot)
		.replace("${name}", name)
	$("#players").append(block)

	player = players[slot]
	if player == undefined
		player =
			slot: slot,
			status: 1,
			score: "?"
	updatePlayerDisplay(player)

updatePlayer = (slot, statusScore) ->
	player = players[slot] =
		slot: slot,
		status: statusScore & 1
		score: (statusScore & ~1) / 2
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

connectServer = (url, f, enter) ->
	ws = new WebSocket(url)
	ws.onopen = (e) ->
		$(".gameOver").hide()
		f()
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
					newPlayer(slot, e.data.substring(2))
			else if code == 97 # player enter
				currentSlot = gameCodecDecode(e.data[1])
				enter(currentSlot)
			else if code == 98 # player leave
				removePlayer(gameCodecDecode(e.data[1]))
			else if code == 101 # stats
				updateStats(gameCodecDecode(e.data[1]), gameCodecDecode(e.data[2]))
			else
				tick(e.data)
	ws.onerror = (e) -> alert("error: "+ e.data)

	spawn: (name) -> ws.send(gameCodecEncode(96) + name)
	command: (vector) -> ws.send(gameCodecEncode(102 + vector))
	close: -> ws.close()

init = ->
	map = (0 for y in [0..(gridSize - 1)] for x in [0..(gridSize - 1)])

	canvas = document.getElementById "game"
	canvas.width = canvas.width;
	ctx = canvas.getContext "2d"

	ctx.fillStyle = "#27005b"
	ctx.fillRect 0, 0, gridSize * 10, gridSize * 10

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
		server.spawn(name)
		return false
