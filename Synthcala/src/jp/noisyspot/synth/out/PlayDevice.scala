package jp.noisyspot.synth.out

import javax.sound.sampled.{AudioFormat, Mixer, AudioSystem, SourceDataLine}
import jp.noisyspot.synth.SoundSystemConsts._
import scala.Some
import ByteConverterFuncs._

final class SourceDataLineOut(val line: SourceDataLine) extends AudioOut {
  def write(b: Array[Byte], off: Int, len: Int) = line.write(b, off, len)

  def open(format: AudioFormat) {
    line.open(format)
  }

  def flush() {
    line.flush()
  }

  def start() {
    line.start()
  }

  def close() {
    line.stop()
    line.close()
  }
}

object PlayDevice {
  def apply(frameRate: Double,
            channels: Int,
            theMixer: Option[Mixer.Info],
            theByteConverterFunc: ByteConvType) = {

    /**サウンドデバイスへのデータラインを取得 */
    val form = OutDevice.getFormat(frameRate, channels)
    val out = new SourceDataLineOut(theMixer match {
      case Some(x) => AudioSystem.getSourceDataLine(form, x)
      case None => AudioSystem.getSourceDataLine(form)
    })
    OutDevice(out, frameRate, channels, theByteConverterFunc)
  }

  /**デフォルト(SoundSystemConstsに定義された設定)のサウンドデバイスを取得します */
  def default() = apply(FRAMES_PAR_SEC, CHANNELS, None, bc16bit)
}
