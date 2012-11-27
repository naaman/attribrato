package controllers

import play.api.mvc._
import services._
import play.api.data._
import play.api.data.Forms._
import java.util.concurrent.TimeUnit
import services.LibratoUser
import services.MetricAttributes
import play.api.libs.concurrent.Promise
import play.api.Logger

object Application extends Controller {

  def index = LibratoAuthenticated { librato => implicit lu => request =>
    librato.getAllMetrics.map(metrics => Ok(views.html.index(metrics)))
  }

  def editMetricsAttributes = LibratoAuthenticated(parse.urlFormEncoded) {
    librato => implicit lu => request =>

    librato.getMetrics(request.body("metric")).map { metrics =>
      def mergeAttr(left: Option[String], right: Option[String]) =
        for { l <- left; r <- right if l == r } yield (r)

      def mergeAttributes(left: MetricAttributes, right: MetricAttributes) =
        MetricAttributes(
          mergeAttr(left.color, right.color),
          mergeAttr(left.displayMax, right.displayMax),
          mergeAttr(left.displayMin, right.displayMin),
          mergeAttr(left.displayUnitsLong, right.displayUnitsLong),
          mergeAttr(left.displayUnitsShort, right.displayUnitsShort),
          mergeAttr(left.displayStacked, right.displayStacked),
          mergeAttr(left.displayTransform, right.displayTransform)
        )

      val mergedAttributes = metrics.map(_.attributes).reduceLeft(mergeAttributes(_, _))

      Ok(views.html.editmetric(metrics, attributesForm.fill(mergedAttributes)))
    }
  }

  def saveMetricsAttributes = LibratoAuthenticated(parse.urlFormEncoded) {
    librato => implicit lu => implicit request =>

    val newAttrs = attributesForm.bindFromRequest()
    Logger.debug("newAttrs: " + newAttrs)
    librato.putMetricAttributes(request.body("metric"), newAttrs.get).map { m =>
      Redirect(routes.Application.index)
    }.recover {
      case e: Exception =>
        Logger.error("Unable to save metrics.", e)
        BadRequest
    }
  }

  val attributesForm = Form(
    mapping(
      "color" -> optional(text),
      "displayMax" -> optional(text),
      "displayMin" -> optional(text),
      "displayUnitsLong" -> optional(text),
      "displayUnitsShort" -> optional(text),
      "displayStacked" -> optional(text),
      "displayTransform" -> optional(text)
    )
    (MetricAttributes.apply)
    (MetricAttributes.unapply)
  )

}

object Librato extends Controller {
  import com.codahale.jerkson.Json

  def metrics = LibratoAuthenticated { librato => implicit lu => request =>
    librato.getMetrics.map(Json.generate(_)).map(Ok(_))
  }

  def allMetrics = LibratoAuthenticated { l => implicit lu => req =>
    l.getAllMetrics.map(Json.generate(_)).map(Ok(_))
  }
}

object Login extends Controller {
  def login = Action {
    Ok(views.html.login(loginForm))
  }

  def authenticate = Action { implicit req =>
    loginForm.bindFromRequest().fold(
      error    => Unauthorized(views.html.login(error)),
      userInfo => Redirect(routes.Application.index()).withSession(
        "librato.username" -> userInfo.username,
        "librato.password" -> userInfo.password
      )
    )
  }

  val loginForm = Form (
    mapping(
      "username" -> email,
      "password" -> nonEmptyText
    )
    (LibratoUser.apply)
    (LibratoUser.unapply)
    verifying(
      "Invalid username or password.",
      validateLogin
    )
  )

  protected def validateLogin = (lu: LibratoUser) =>
    new PlayAppLibratoService(lu)
      .getMetrics
      .map(success)
      .recover(recoverAuth)
      .await(5L, TimeUnit.SECONDS)
      .get

  protected def success = (result: Any) => true

  protected def recoverAuth: PartialFunction[Throwable, Boolean] = {
    case e: RequestFailedException if (e.getStatus == UNAUTHORIZED) => false
  }
}