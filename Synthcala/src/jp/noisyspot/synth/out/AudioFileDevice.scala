package jp.noisyspot.synth.out

import javax.sound.sampled._
import java.io._
import jp.noisyspot.synth.out.ByteConverterFuncs._
import jp.noisyspot.synth.SoundSystemConsts._
import scala.Some
import scala.Some
import javax.management.remote.rmi._RMIConnection_Stub

final class WavAudioFileOut(val targetFile: File) extends AudioOut {

  private var tempFile : Option[File] = None
  private var tempStream : Option[OutputStream] = None
  private var format: Option[AudioFormat] = None

  def write(b: Array[Byte], start: Int, length: Int) = {
    try{
      tempStream.foreach(_.write(b, start, length))
      length
    } catch {
      case e: IOException => {e.printStackTrace(System.err); -1}
    }
  }

  def open(f: AudioFormat) {format = Option(f)}

  def start() {
    tempFile = Some(File.createTempFile("synthcala",".tmp")).map(x => {x.deleteOnExit(); x})
    tempStream = tempFile.map(new FileOutputStream(_))
  }

  def flush() {tempStream.foreach(_.flush())}

  def close() {finishing()}

  private def finishing() {
    tempStream.foreach(_.close())
    (format, tempFile) match {
      case (Some(f), Some(file)) => {
        val in = new AudioInputStream(new FileInputStream(file), f, file.length())
        AudioSystem.write(in, AudioFileFormat.Type.WAVE, targetFile)
        in.close()
      }
      case _ => ()
    }
    tempStream = None
    tempFile = None
  }
}

object WavAudioFileOut {
  def apply(targetFile: File) =
    Option(targetFile).filter(x => (x.createNewFile() || x.canWrite)).map(new WavAudioFileOut(_))
}

object AudioFileDevice {
  def apply(targetFile: File, frameRate: Double, channels: Int, theByteConverterFunc: ByteConvType) = {

    val out = WavAudioFileOut(targetFile).getOrElse(throw new IllegalArgumentException("targetFile is not valie"))
    OutDevice(out, frameRate, channels, theByteConverterFunc)
  }

  /**デフォルト(SoundSystemConstsに定義された設定)のサウンドデバイスを取得します */
  def default(targetFile: File) = apply(targetFile, FRAMES_PAR_SEC, CHANNELS, bc16bit)

}
