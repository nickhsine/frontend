package test

import controllers.SeriesController
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, FlatSpec, Matchers}
import play.api.test.Helpers._

@DoNotDiscover class SeriesControllerTest
  extends FlatSpec
  with Matchers
  with ConfiguredTestSuite
  with BeforeAndAfterAll
  with WithTestWsClient
  with WithTestContentApiClient {

  var series = "news/series/pass-notes"
  lazy val seriesController = new SeriesController(testContentApiClient)

  "Series Controller" should "200 when content type is a tag" in {
    val result = seriesController.renderSeriesStories(series)(TestRequest())
    status(result) should be (200)
  }

}
