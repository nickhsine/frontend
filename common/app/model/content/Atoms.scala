package model.content

import com.gu.contentapi.client.model.{v1 => contentapi}
import com.gu.contentatom.thrift.{atom => atomapi, Atom => ThriftAtom, AtomData}
import model.{ImageAsset, ImageMedia}
import play.api.libs.json.{JsError, JsSuccess, Json}
import quiz._

final case class Atoms(
  quizzes: Seq[Quiz],
  interactives: Seq[InteractiveAtom]
) {
  val all: Seq[Atom] = quizzes ++ interactives
}

sealed trait Atom {
  def id: String
}

final case class Quiz(
  override val id: String,
  title: String,
  path: String,
  quizType: String,
  content: QuizContent,
  revealAtEnd: Boolean
) extends Atom

final case class InteractiveAtom(
  override val id: String,
  `type`: String,
  title: String,
  css: String,
  html: String,
  inlineJS: Option[String],
  mainJS: Option[String],
  docData: Option[String]
) extends Atom

object Atoms extends common.Logging {
  def extract[T](atoms: Option[Seq[ThriftAtom]], extractFn: ThriftAtom => T): Seq[T] = {
    try {
      atoms.getOrElse(Nil).map(extractFn)
    } catch {
      case e: Exception =>
        logException(e)
        Nil
    }
  }

  def make(content: contentapi.Content): Option[Atoms] = {
    content.atoms.map { atoms =>
      val quizzes = extract(atoms.quizzes, atom => {
        val quizData = atom.data.asInstanceOf[AtomData.Quiz].quiz
        Quiz.make(content.id, quizData)
      })
      val interactives = extract(atoms.interactives, atom => {
        val interactiveData = atom.data.asInstanceOf[AtomData.Interactive].interactive
        InteractiveAtom.make(atom.id, interactiveData)
      })

      Atoms(quizzes = quizzes, interactives = interactives)
    }
  }
}

object Quiz extends common.Logging {

  implicit val assetFormat = Json.format[Asset]
  implicit val imageFormat = Json.format[Image]

  private def transformAssets(quizAsset: Option[atomapi.quiz.Asset]): Option[QuizImageMedia] = quizAsset.flatMap { asset =>
    val parseResult = Json.parse(asset.data).validate[Image]
    parseResult match {
      case parsed: JsSuccess[Image] => {
        val image = parsed.get
        val typeData = image.fields.mapValues(value => value.toString) - "caption"

        val assets = for {
          plainAsset <- image.assets
        } yield {
         ImageAsset(
          index = 0,
          fields = typeData ++ plainAsset.fields.mapValues(value => value.toString),
          mediaType = plainAsset.assetType,
          mimeType = plainAsset.mimeType,
          url = plainAsset.secureUrl.orElse(plainAsset.url))
        }
        if (assets.nonEmpty) Some(QuizImageMedia(ImageMedia(allImages = assets))) else None
      }
      case error: JsError => {
        log.warn("Quiz atoms: asset json read errors: " + JsError.toFlatForm(error).toString())
        None
      }
    }
  }

  def make(path: String, quiz: atomapi.quiz.QuizAtom): Quiz = {
    val questions = quiz.content.questions.map { question =>
      val answers = question.answers.map { answer =>
        Answer(
          id = answer.id,
          text = answer.answerText,
          revealText = answer.revealText.flatMap(revealText => if (revealText != "") Some(revealText) else None),
          weight = answer.weight.toInt,
          buckets = answer.bucket.getOrElse(Nil),
          imageMedia = transformAssets(answer.assets.headOption))
      }

      Question(
        id = question.id,
        text = question.questionText,
        answers = answers,
        imageMedia = transformAssets(question.assets.headOption))
    }

    val content = QuizContent(
      questions = questions,
      resultGroups = quiz.content.resultGroups.map(resultGroups => {
        resultGroups.groups.map(resultGroup => {
          ResultGroup(
            id = resultGroup.id,
            title = resultGroup.title,
            shareText = resultGroup.share,
            minScore = resultGroup.minScore
          )
        })
      }).getOrElse(Nil),
      resultBuckets = quiz.content.resultBuckets.map(resultBuckets =>{
        resultBuckets.buckets.map(resultBucket => {
          ResultBucket(
            id = resultBucket.id,
            title = resultBucket.title,
            shareText = resultBucket.share,
            description = resultBucket.description
          )
        })
      }).getOrElse(Nil)
    )

    Quiz(
      id = quiz.id,
      path = path,
      title = quiz.title,
      quizType = quiz.quizType,
      content = content,
      revealAtEnd = quiz.revealAtEnd
    )
  }
}

object InteractiveAtom {
  def make(id: String, interactive: atomapi.interactive.InteractiveAtom): InteractiveAtom = {
    InteractiveAtom(
      id = id,
      `type` = interactive.`type`,
      title = interactive.title,
      css = interactive.css,
      html = interactive.html,
      inlineJS = interactive.inlineJS,
      mainJS = interactive.mainJS,
      docData = interactive.docData
    )
  }
}
