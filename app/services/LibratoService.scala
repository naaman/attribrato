package services

import play.api.libs.ws.WS
import org.jboss.netty.handler.codec.http.HttpMethod
import com.ning.http.client.Realm
import play.api.libs.ws.WS.WSRequestHolder
import com.codahale.jerkson.Json
import play.api.http.Status
import play.api.libs.concurrent.Promise

/**
 * @author Naaman Newbold
 */
class PlayAppLibratoService(un: String, pw: String)
  extends LibratoService {

  def client = new LibratoClient {
    def config = new LibratoConfig {
      import play.api.Play.current
      import play.api.Play.configuration
      val host = configuration
        .getString("librato.host")
        .getOrElse("metrics-api.librato.com")

      val username = un
      val password = pw
    }
  }

}

trait LibratoService {
  def client: LibratoClient

  def getMetrics: Promise[Map[String, Any]] = client.exec(new MetricList)
}

trait LibratoRequest[T] {
  def method: HttpMethod
  def path: String
  def body: Option[String]
  def respond(status: Int, data: Array[Byte]): T
}

trait LibratoConfig {
  def host: String
  def protocol: String = "https"
  def username: String
  def password: String
}

trait LibratoClient {
  def config: LibratoConfig

  def exec[T](implicit req: LibratoRequest[T]): Promise[T] =
    createWS(req).map(r => req.respond(r.status, r.body.getBytes))

  protected def createWS[T] = callout[T] _ compose auth[T] compose url[T]

  protected def auth[T](r: (WSRequestHolder, LibratoRequest[T])) =
    (r._1.withAuth(config.username, config.password, Realm.AuthScheme.BASIC), r._2)

  protected def url[T](req: LibratoRequest[T]): (WSRequestHolder, LibratoRequest[T]) =
    (WS.url(config.protocol + "://" + config.host + "/v1" + req.path), req)

  protected def callout[T](reqs: (WSRequestHolder, LibratoRequest[T])) = {
    val (wsReq, req) = reqs
    req.method match {
      case HttpMethod.GET => wsReq.get()
      case HttpMethod.POST => wsReq.post(req.body.getOrElse(""))
      case HttpMethod.PUT => wsReq.put(req.body.getOrElse(""))
      case HttpMethod.DELETE => wsReq.delete()
    }
  }
}

class MetricList extends LibratoRequest[Map[String, Any]] {
  def method = HttpMethod.GET
  def path = "/metrics"
  def body = None
  def respond(status: Int, data: Array[Byte]) = {
    if (status == Status.OK) Json.parse[Map[String, Any]](data)
    else throw new RequestFailedException(status, data)
  }
}

class RequestFailedException(status: Int, body: Array[Byte])
  extends RuntimeException(
    "Request failed: Status=%d, Body=%s".format(status, String.valueOf(body))
  ) {
  val getStatus = status
  val getBody = body
}