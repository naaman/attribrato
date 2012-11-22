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
class PlayAppLibratoService(libratoUser: LibratoUser) extends LibratoService {
  def client = new LibratoClient {
    def config = new LibratoConfig {
      import play.api.Play.current
      import play.api.Play.configuration
      val host = configuration
        .getString("librato.host")
        .getOrElse("metrics-api.librato.com")

      val username = libratoUser.username
      val password = libratoUser.password
    }
  }

  def user = libratoUser
}

case class LibratoUser(username: String, password: String)

trait LibratoService {
  def client: LibratoClient
  def user: LibratoUser

  def getMetrics: Promise[MetricQuery] = client.exec(new MetricList(100))

  def getAllMetrics = {
    val batch = 100

    def metricSlices(total: Int): Seq[(Int, Int)] = {
      val batches = for (i <- 0 to (total / batch - 1)) yield (i * 100, batch)
      if (total % batch == 0) batches
      else batches :+ ((total / batch) * batch -> total % batch)
    }

    def execBatches = (batches: Seq[(Int, Int)]) => batches.map {
      case (offset, total) => client.exec(new MetricList(total, offset))
    }

    def sequenceBatches(batches: Seq[(Int, Int)]) = Promise.sequence(execBatches(batches))

    client.exec(new MetricList(1))
      .map(_.query("total"))
      .map(metricSlices(_))
      .flatMap(sequenceBatches(_))
      .map(_.map(_.metrics).flatten)
  }
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

case class MetricQuery(query: Map[String, Int], metrics: Seq[Map[String, Any]])

class MetricList(length: Int, offset: Int = 0) extends LibratoRequest[MetricQuery] {
  def method = HttpMethod.GET
  def path = "/metrics?length=" + length + "&offset=" + offset
  def body = None
  def respond(status: Int, data: Array[Byte]) = {
    if (status == Status.OK) Json.parse[MetricQuery](data)
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