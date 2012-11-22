package controllers

import services.{LibratoUser, PlayAppLibratoService, LibratoService}
import play.api.mvc._
import play.api.libs.concurrent.Promise
import play.api.mvc.AsyncResult

/**
 * @author Naaman Newbold
 */

object LibratoAuthenticated {
  def apply(ƒ: LibratoService => Option[LibratoUser] => Request[AnyContent] => Promise[Result]):
    Action[AnyContent] = apply(BodyParsers.parse.anyContent)(ƒ)

  def apply[A](bodyParser: BodyParser[A])
              (ƒ: LibratoService => Option[LibratoUser] => Request[A] => Promise[Result]) =
    new Action[A] {
      import play.api.mvc.Results.Redirect

      def parser = bodyParser

      def apply(request: Request[A]): Result = {
        creds(request.session).map {
          case lu: LibratoUser =>
            AsyncResult(ƒ(new PlayAppLibratoService(lu))(Some(lu))(request))
        }.getOrElse(Redirect(routes.Login.login()))
      }

      protected def creds(session: Session): Option[LibratoUser] = {
        for {
          username <- session.get("librato.username")
          password <- session.get("librato.password")
        } yield (LibratoUser(username, password))
      }
    }
}

class AuthenticationException(msg: String) extends RuntimeException(msg)