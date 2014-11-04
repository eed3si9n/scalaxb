package scalaxb.stockquote.server

import javax.jws.{ WebService, WebMethod, WebParam, WebResult }
import javax.jws.soap.SOAPBinding
import javax.jws.soap.SOAPBinding.{Style, Use, ParameterStyle}
import collection.mutable

@WebService(name = "RpcLiteralService", serviceName = "RpcLiteralService")
@SOAPBinding(style = Style.RPC, use = Use.LITERAL, parameterStyle = ParameterStyle.WRAPPED)
class RpcLiteralService {
  private val buffer = mutable.Map[String, Double]()

  def price(symbol: String): Double =
    buffer.getOrElse(symbol, 42.0)

  def update(symbol: String, price: Double): Unit =
    buffer(symbol) = price

  @WebMethod(operationName = "useHeader", action = "useHeader")
  @WebResult(header = true)
  def useHeader(@WebParam(header = true) symbol: String): Double = price(symbol)

  def infos: Array[String]= Array("x")
}
