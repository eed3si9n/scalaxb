package scalaxb.specs

import javax.ws.rs.{GET, Path, PathParam, Produces}
import javax.xml.bind.annotation.XmlRootElement
import scala.beans.BeanProperty

class RestService {
  @GET
  @Path("/item/{symbol}")
  @Produces(Array("application/xml"))
  def item(@PathParam("symbol") symbol: String): StoreItem = StoreItem(symbol, 42.0)
}

@XmlRootElement
case class StoreItem(@BeanProperty var symbol: String,
                     @BeanProperty var price: Double) {
  def this() = this(null, -1)
}