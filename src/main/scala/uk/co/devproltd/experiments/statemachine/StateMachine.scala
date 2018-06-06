package uk.co.devproltd.experiments.statemachine
import java.time.LocalDateTime

sealed trait AttachmentState

case object Initiating extends AttachmentState
case object InitiationFailed extends AttachmentState
case object AwaitingScanResults extends AttachmentState
case object Rejected extends AttachmentState
case object ReadyForUpload extends AttachmentState
case object Uploading extends AttachmentState
case object UploadFailed extends AttachmentState
case object UploadCompleted extends AttachmentState

case class AttachmentEvent(newState: AttachmentState, timestamp: LocalDateTime = LocalDateTime.now())

case class Attachment[S <: AttachmentState](
  fileName: String,
  mimeType: String,
  state: AttachmentState,
  history: List[AttachmentEvent]
){
  override def toString(): String =
    s"""Attachment(
      | fileName=$fileName,
      | mimeType=$mimeType,
      | state=$state,
      | history=${history.map(_.toString).mkString("\t", "\n\t\t", "\n")}""".stripMargin
}

object Attachment{
  def newAttachment:Attachment[Initiating.type] =
    Attachment(fileName="foo", mimeType="bar", state=Initiating, history=List.empty)
  def failInitiation(in: Attachment[AwaitingScanResults.type]):Attachment[InitiationFailed.type] =
    transitionTo(in, InitiationFailed)
  def awaitScanResult(in: Attachment[Initiating.type]):Attachment[AwaitingScanResults.type] =
    transitionTo(in, AwaitingScanResults)
  def reject(in: Attachment[AwaitingScanResults.type]):Attachment[Rejected.type] =
    transitionTo(in, Rejected)
  def readyForUpload(in: Attachment[AwaitingScanResults.type]): Attachment[ReadyForUpload.type] =
    transitionTo(in, ReadyForUpload)
  def upload(in:Attachment[ReadyForUpload.type]): Attachment[Uploading.type] =
    transitionTo(in, Uploading)
  def failUpload(in:Attachment[Uploading.type]): Attachment[UploadFailed.type] =
    transitionTo(in, UploadFailed)
  def completeUpload(in:Attachment[Uploading.type]): Attachment[UploadCompleted.type] =
    transitionTo(in, UploadCompleted)
  def retry(in:Attachment[Uploading.type]): Attachment[ReadyForUpload.type] =
    transitionTo(in, ReadyForUpload)

  private def transitionTo[S <: AttachmentState](a: Attachment[_], newState:S): Attachment[S] =
    a.copy(state=newState, history=AttachmentEvent(newState) :: a.history)
}

object StateMachine extends App {

  implicit class pipeOps[T](value: T){
    def |>[B](f: T => B): B = f(value)
  }

  import Attachment._

  println(newAttachment |> awaitScanResult |> readyForUpload |> upload |> completeUpload )
  println(newAttachment |> awaitScanResult |> readyForUpload |> upload |> failUpload )
  println(newAttachment |> awaitScanResult |> readyForUpload |> upload |> retry |> upload |> retry |> upload |> completeUpload )

}

