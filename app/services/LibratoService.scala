package services

import play.api.libs.ws.WS
import org.jboss.netty.handler.codec.http.HttpMethod
import com.ning.http.client.Realm
import play.api.libs.ws.WS.WSRequestHolder
import com.codahale.jerkson.{JsonSnakeCase, Json}
import play.api.http.{Status, HeaderNames}
import play.api.libs.concurrent.Promise
import play.api.Logger

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

    client.exec(new MetricList(1))
      .map(_.query("total"))
      .map(metricSlices(_))
      .flatMap(client.collectBatch(_) {
        case (offset, total) => new MetricList(total, offset)
      })
      .map(_.map(_.metrics).flatten)
  }

  def getMetrics(metrics: Seq[String]) = {
    client.collectBatch(metrics)(new MetricInfo(_))
  }

  def putMetricAttributes(metrics: Seq[String], attrs: MetricAttributes) = {
    client.collectBatch(metrics) { metricName =>
      new AttributesUpdate(metricName, attrs)
    }
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

  def collectBatch[T,R](req: Seq[T])(ƒ: T => LibratoRequest[R]) =
    Promise.sequence(execBatch(req)(ƒ))

  def execBatch[T,R](req: Seq[T])(ƒ: T => LibratoRequest[R]) =
    req.map(t => exec(ƒ(t)))

  def exec[T](implicit req: LibratoRequest[T]): Promise[T] =
    createWS(req).map(r => req.respond(r.status, r.body.getBytes))

  protected def createWS[T] = callout[T] _ compose auth[T] compose headers[T] compose url[T]

  protected def url[T](req: LibratoRequest[T]): (WSRequestHolder, LibratoRequest[T]) =
    (WS.url(config.protocol + "://" + config.host + "/v1" + req.path), req)

  protected def auth[T](r: (WSRequestHolder, LibratoRequest[T])) =
    (r._1.withAuth(config.username, config.password, Realm.AuthScheme.BASIC), r._2)

  protected val stdHeaders = Seq(HeaderNames.CONTENT_TYPE -> "application/json")
  protected def headers[T](r: (WSRequestHolder, LibratoRequest[T])) =
    (r._1.withHeaders(stdHeaders:_*), r._2)

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

case class MetricQuery(query: Map[String, Int], metrics: Seq[LibratoMetric])

class MetricList(length: Int, offset: Int = 0) extends LibratoRequest[MetricQuery] {
  def method = HttpMethod.GET
  def path = "/metrics?length=" + length + "&offset=" + offset
  def body = None
  def respond(status: Int, data: Array[Byte]) = {
    if (status == Status.OK) Json.parse[MetricQuery](data)
    else throw new RequestFailedException(status, data)
  }
}

@JsonSnakeCase
case class LibratoMetric(
  name: String,
  displayName: String,
  `type`: String,
  description: Option[String],
  period: Option[String],
  attributes: MetricAttributes,
  measurements: Option[Map[String, Any]]
)

@JsonSnakeCase
case class MetricAttributes(
  color: Option[String],
  displayMax: Option[String],
  displayMin: Option[String],
  displayUnitsLong: Option[String],
  displayUnitsShort: Option[String],
  displayStacked: Option[String],
  displayTransform: Option[String]
)

class MetricInfo(name: String) extends LibratoRequest[LibratoMetric] {
  def method = HttpMethod.GET
  def path = "/metrics/" + name
  def body = None
  def respond(status: Int, data: Array[Byte]) = {
    if (status == Status.OK) Json.parse[LibratoMetric](data)
    else throw new RequestFailedException(status, data)
  }
}

class AttributesUpdate(metricName: String, attrs: MetricAttributes)
  extends LibratoRequest[Unit] {
  def method = HttpMethod.PUT
  def path = "/metrics/" + metricName
  def body = Some(Json.generate(Map("attributes" -> attrs)))
  def respond(status: Int, data: Array[Byte]) = {
    if (status == Status.NO_CONTENT) Unit
    else throw new RequestFailedException(status, data)
  }
}

class RequestFailedException(status: Int, body: Array[Byte])
  extends RuntimeException(
    "Request failed: Status=%d, Body=%s".format(status, new String(body))
  ) {
  val getStatus = status
  val getBody = body
}