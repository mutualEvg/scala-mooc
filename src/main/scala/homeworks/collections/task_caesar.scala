package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  val upper = 'A' to 'Z'
  val indexedUpper = upper.zipWithIndex.toMap.map(entry => (entry._1, entry._2 + 1))
  val swapIndexedUpper = indexedUpper.map(_.swap)
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String =
    word.map(string => getValue(indexedUpper(string), offset)).map(number => swapIndexedUpper(number)).mkString

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    cipher.map(string => getDecriptedValue(indexedUpper(string), offset)).map(number => swapIndexedUpper(number)).mkString

  def getValue(strindex: Int, offset: Int): Int = {
    if (offset > 26 && (strindex + (offset%26)) > 26){
      (strindex + (offset%26))%26
    } else if (offset > 26 && (strindex + (offset%26)) <= 26) {
      strindex + (offset%26)
    } else if (offset < 26 && (strindex + (offset%26)) > 26) {
      strindex + offset - 26
    } else strindex + offset
  }
  def getDecriptedValue(strindex: Int, offset: Int): Int =
    if (offset > 26) {
      val x = offset-offset/26*26
      if (strindex - x < 1){
        strindex - x + 26 }
      else strindex - x
    } else {
      if (strindex - offset < 1){ strindex - offset + 26 }
      else strindex - offset
    }
}
object task_caesar2 {

  private val ALPHABET: IndexedSeq[Char] = 'A' to 'Z'

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = word.map(encryptChar(offset))

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = encrypt(cipher, -offset)

  private def encryptChar(offset: Int)(char: Char): Char = {
    val index = calculateIndex(char, offset)
    if (index >= 0) {
      ALPHABET(index)
    } else {
      ALPHABET(index + ALPHABET.size)
    }
  }

  private def calculateIndex(char: Char, offset: Int) = (ALPHABET.indexOf(char) + offset) % ALPHABET.size
}