package feed

import app.LifecycleComponent
import common.AutoRefresh
import play.api.inject.ApplicationLifecycle
import play.api.{Application, GlobalSettings}
import services.{MostReadItem, OphanApi}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class MostReadSocial(twitter: Seq[MostReadItem], facebook: Seq[MostReadItem])

class MostPopularSocialAutoRefresh(ophanApi: OphanApi) extends AutoRefresh[MostReadSocial](0.seconds, 3.minutes) {
  val Hours = 3

  override protected def refresh(): Future[MostReadSocial] = {
    for {
      facebookMostRead <- ophanApi.getMostReadFacebook(Hours)
      twitterMostRead <- ophanApi.getMostReadTwitter(Hours)
    } yield MostReadSocial(twitterMostRead, facebookMostRead)
  }
}

class MostPopularFacebookAutoRefreshLifecycle(appLifeCycle: ApplicationLifecycle,
                                              mostPopularSocialAutoRefresh: MostPopularSocialAutoRefresh)
                                             (implicit ec: ExecutionContext) extends LifecycleComponent {

  appLifeCycle.addStopHook { () => Future {
    mostPopularSocialAutoRefresh.stop()
  }}

  override def start(): Unit = {
    mostPopularSocialAutoRefresh.start()
  }
}
