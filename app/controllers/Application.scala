package controllers

import play.api.mvc._
import services.{RequestFailedException, PlayAppLibratoService, LibratoService}
import play.api.libs.concurrent.Promise
import play.api.data._
import play.api.data.Forms._
import java.util.concurrent.TimeUnit
import play.api.Logger

object Application extends Controller {

  case class LibratoUser(username: String, password: String)
  val loginForm = Form(
    mapping("username" -> email, "password" -> nonEmptyText)
    (LibratoUser.apply)(LibratoUser.unapply)
    verifying("Invalid username or password.", lu => lu match {
      case LibratoUser(u, p) =>
        new PlayAppLibratoService(u,p)
          .getMetrics
          .map{r => Logger.debug("response " + r); true}
          .recover {
            case rfe: RequestFailedException if (rfe.getStatus == UNAUTHORIZED) => false
          }.await(5L, TimeUnit.SECONDS).get
    })
  )

  def login = Action {
    Ok(views.html.login(loginForm))
  }

  def authenticate = Action { implicit req =>
    loginForm.bindFromRequest().fold(
      error    => Unauthorized(views.html.login(error)),
      userInfo => Redirect(routes.Application.index())
                    .withSession(
                      "librato.username" -> userInfo.username,
                      "librato.password" -> userInfo.password)
    )
  }

  def index = LibratoAuthenticated { librato => request =>
    Promise.pure(Ok(views.html.index("Your new application is ready.")))
  }

}