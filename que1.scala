object CaesarCipher {

  def encrypt(plainText: String, shift: Int): String = {
    val encryptedText = plainText.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base + shift) % 26 + base).toChar
      } else {
        char
      }
    }
    encryptedText
  }

  def decrypt(encryptedText: String, shift: Int): String = {
    val decryptedText = encryptedText.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base - shift + 26) % 26 + base).toChar
      } else {
        char
      }
    }
    decryptedText
  }

  def main(args: Array[String]): Unit = {
    val shift = 3 // You can change the shift value as per your requirement
    val plainText = "Hello, World!" // Replace this with your plaintext

    val encryptedText = encrypt(plainText, shift)
    println(s"Encrypted Text: $encryptedText")

    val decryptedText = decrypt(encryptedText, shift)
    println(s"Decrypted Text: $decryptedText")
  }
}
