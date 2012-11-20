package controllers

import services.{PlayAppLibratoService, LibratoService}
import play.api.mvc._
import play.api.libs.concurrent.Promise
import play.api.mvc.AsyncResult

/**
 * @author Naaman Newbold
 */

object LibratoAuthenticated {
  def apply(ƒ: LibratoService =>
               Request[AnyContent] =>
               Promise[Result]): Action[AnyContent] =
    apply(BodyParsers.parse.anyContent)(ƒ)

  def apply[A](bodyParser: BodyParser[A])
              (ƒ: LibratoService =>
                  Request[A] =>
                  Promise[Result]) = new Action[A] {
    import play.api.mvc.Results.Redirect

    def parser = bodyParser

    def apply(request: Request[A]): Result = {
      creds(request.session).map {
        case (username, password) =>
          AsyncResult(ƒ(new PlayAppLibratoService(username, password))(request))
      }.getOrElse(Redirect(routes.Application.login()))
    }

    protected def creds(session: Session): Option[(String, String)] = {
      for {
        username <- session.get("librato.username")
        password <- session.get("librato.password")
      } yield (username, password)
    }
  }
}

class AuthenticationException(msg: String) extends RuntimeException(msg)