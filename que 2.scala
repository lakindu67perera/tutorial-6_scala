object CaesarCipher {

  def encrypt(text: String, shift: Int): String = {
    val encryptedText = text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        val shiftedChar = (char - base + shift) % 26
        val wrappedChar = (if (shiftedChar < 0) shiftedChar + 26 else shiftedChar) + base
        wrappedChar.toChar
      } else {
        char
      }
    }
    encryptedText
  }

  def decrypt(encryptedText: String, shift: Int): String = {
    encrypt(encryptedText, 26 - shift)
  }


  def cipher(text: String, shift: Int, operation: String => String): String = {
    operation(text)
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"
    val shift = 2


    val encrypted = cipher(plaintext, shift, encrypt)
    println(s"Encrypted text: $encrypted")


    val decrypted = cipher(encrypted, shift, decrypt)
    println(s"Decrypted text: $decrypted")
  }
}
