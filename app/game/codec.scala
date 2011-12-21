package game.codec

import game.Entity

object Codec {
	val base = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-+"

	def encode(num: Int, size: Int): String = {
		assert(num >> (size * 6) == 0)
		0.until(size).foldLeft("")((code, i) => code + base((num >> (i * 6)) & 0x3f))
	}

	def decode(code: String): Int = {
		code.reverse.foldLeft(0) { (n, ch) =>
			val j = base.indexOf(ch)
			if (j == -1) throw new IllegalArgumentException("Invalide code "+ code)
			(n << 6) | j
		}
	}
}

trait CodeChunk {
	def codeChunk: String
}

trait CodeElement extends CodeChunk {
	def elementCode: String = codeChunk
}
