ctx = map = null
gridSize = 40

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
	128: # food
		r: 0
		g: 127
		b: 0
	129: # eatenFood
		r: 0
		g: 192
		b: 0
	130: # exploded
		r: 255
		g: 0
		b: 0
	131: # evilSnake
		r: 192
		g: 0
		b: 0

rgb = (item) -> "rgb(" + item.r + "," + item.g + "," + item.b + ")"
rgba = (item, alpha) -> "rgba(" + item.r + "," + item.g + "," + item.b + ", " + alpha + ")"

gameCodecEncode = (num) ->
	if num < 0 || num >= 255
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
		if entity.id != 0
			item = Item[entity.id]
			if item == undefined
				item = Item[0]
			drawBlock(entity.pos, item)
		else
			eraseBlock entity.pos

connectServer = (f) ->
	ws = new WebSocket(snakeGameWebsocket)
	ws.onopen = (e) -> f()
	ws.onclose = (e) ->
		$(".gameOver .inner").text("La connexion au serveur a été fermé")
		$(".gameOver").show()
	ws.onmessage = (e) -> tick(e.data)
	ws.onerror = (e) -> alert("error: "+ e.data)

	command: (vector) -> ws.send(gameCodecEncode(vector))

init = ->
	map = (0 for y in [0..(gridSize - 1)] for x in [0..(gridSize - 1)])

	canvas = document.getElementById "game"
	canvas.width = canvas.width;
	ctx = canvas.getContext "2d"

	ctx.fillStyle = "#27005b"
	ctx.fillRect 0, 0, gridSize * 10, gridSize * 10

$ ->
	server = connectServer ->
		init()

	$(document).keydown (event) ->
		switch event.keyCode
			when KEY.LEFT_ARROW then server.command(3); break;
			when KEY.UP_ARROW then server.command(0); break;
			when KEY.RIGHT_ARROW then server.command(1); break;
			when KEY.DOWN_ARROW then server.command(2); break;

	$(".restart a").click ->
		init()
		$(".gameOver").hide()

	$("#nameModal form").submit ->
		$("#nameModal").hide()
		server.command(254)
		return false