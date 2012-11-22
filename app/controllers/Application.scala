package controllers

import play.api.mvc._
import services.{LibratoUser, RequestFailedException, PlayAppLibratoService}
import play.api.libs.concurrent.Promise
import play.api.data._
import play.api.data.Forms._
import java.util.concurrent.TimeUnit

object Application extends Controller {

  def index = LibratoAuthenticated { librato => implicit lu => request =>
    Promise.pure(Ok(views.html.index("Your new application is ready.")))
  }

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