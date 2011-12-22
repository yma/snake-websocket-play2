package game.codec

import game.Entity

object Codec {
	def encode(num: Int): Char = {
		assert(num >= 0 && num < 255)
		(num + 1).toChar
	}

	def decode(code: Char): Int = {
		val num = code.toInt
		assert(num > 0 && num <= 255)
		num - 1
	}
}

trait CodeChunk {
	def codeChunk: String
}

trait CodeElement extends CodeChunk {
	def elementCode: String = codeChunk
}
