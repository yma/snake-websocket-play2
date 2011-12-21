ctx = map = null
gridSize = 40

KEY =
	LEFT_ARROW: 37
	UP_ARROW: 38
	RIGHT_ARROW: 39
	DOWN_ARROW: 40


gameCodecBase = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-+"
gameCodecEncode = (num, size) ->
	n = num
	buffer = ""
	for i in [0..(size-1)]
		buffer += gameCodecBase.charAt(n & 0x3f)
		n >>= 6
	if n != 0
		alert("Can't encode "+ num +" ("+ size +")")
	buffer

gameCodecDecode = (code) ->
	n = 0
	for i in [(code.length-1)..0]
		j = gameCodecBase.indexOf(code.charAt(i))
		if (j == -1)
			n = j
			break
		n = (n << 6) | j
	if n < 0
		alert("Invalid code "+ code)
	n

decodeEntity = (code) ->
	id: gameCodecDecode(code.substring(0,1))
	pos:
		x: gameCodecDecode(code.substring(1,3))
		y: gameCodecDecode(code.substring(3,5))


drawBlock = (pos) ->
	ctx.strokeStyle = "rgba(255,255,255, 0.5)"
	ctx.strokeRect pos.x * 10 + 0.5, pos.y * 10 + 0.5, 8, 8
	ctx.fillStyle = "rgba(255,255,255,0.2)"
	ctx.fillRect pos.x * 10 + 0.5, pos.y * 10 + 0.5, 8, 8

eraseBlock = (pos) ->
	ctx.fillStyle = "#27005b"
	ctx.fillRect pos.x * 10, pos.y * 10, 10, 10

tick = (gameCode) ->
	while gameCode != ""
		entity = decodeEntity gameCode
		gameCode = gameCode.substring(5)
		if entity.id != 0
			drawBlock entity.pos
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

	command: (vector) -> ws.send(gameCodecEncode(vector, 1))

init = ->
	map = (0 for y in [0..(gridSize - 1)] for x in [0..(gridSize - 1)])

	canvas = document.getElementById "game"
	canvas.width = canvas.width;
	ctx = canvas.getContext "2d"

	ctx.fillStyle = "#27005b"
	ctx.fillRect 0, 0, gridSize * 10, gridSize * 10

	console.log(decodeEntity "12345")

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
