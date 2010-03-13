package org.scalaxb.runtime

/** http://d.hatena.ne.jp/yuroyoro/20100118/1263792194
 * @author yuroyoro
 */
object Base64 {
  val BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  def encode(s: String) = encodeBuffer(s.getBytes)
  def encode(s: String, charset:String) = encodeBuffer(s.getBytes(charset))

  private def encodeBuffer(buf: Array[Byte]) = {
    val bits = fill(buf.map{
        b => if(b < 0) b + 256 else b.toInt
      }.map{
          i => ("00000000" + i.toBinaryString).reverse.take(8).reverse
    }.mkString, "0", 6).toList

    def encodeBits(xs: List[Char]): String = xs match{
      case Nil => ""
      case ss =>
        val (h,t) = ss.splitAt(6)
        BASE64((0 /: h.reverse.zipWithIndex){
            case(n, (c, i)) => n + (Math.pow(2, i) * c.asDigit).toInt
        }) + encodeBits(t)
    }

    fill(encodeBits(bits), "=", 4)
  }

  private def fill(s: String, fill: String, n: Int) = s + (fill * (n - (s.length % n)))

  def decode(s: String) = new String(decodeBuffer(s))
  def decode(s: String , charset: String) = new String(decodeBuffer(s), charset)

  def decodeBuffer(s: String) = {
    val bits = s.map{ BASE64.indexOf(_)}.
      filter(0 <).
      map{
        b => ("000000" + b.toBinaryString).reverse.take(6).reverse
      }.mkString.toList

    def decodeBits(xs: List[Char]): List[Byte]= xs match {
      case Nil => Nil
      case ss =>
        val (h,t) = ss.splitAt(8)
        (((0 /: h.reverse.zipWithIndex){
            case(n, (c, i)) => n + (Math.pow(2, i) * c.asDigit).toInt
        }) match{
          case b if b > 256 => (b - 256).toByte
          case b => b.toByte
        }) :: decodeBits(t)
    }

    decodeBits(bits).toArray
  }
}
