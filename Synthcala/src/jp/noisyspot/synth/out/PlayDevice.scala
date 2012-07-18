package jp.noisyspot.synth.out

import javax.sound.sampled.{AudioFormat, Mixer, AudioSystem, SourceDataLine}
import jp.noisyspot.synth.SoundSystemConsts._
import scala.Some
import ByteConverterFuncs._

class SourceDataLineOut(val line: SourceDataLine) extends AudioOut {
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

  def stop() {
    line.stop()
  }

  def close() {
    line.close()
  }
}

/**
 * 16bitサウンドデバイス用のOutDevice実装クラス
 * @param frameRate フレームレート
 * @param channels チャネル数（モノラル：1 / ステレオ：2)
 * @param theMixer 使用デバイスを指定するMixer.infoオブジェクト。デフォルトデバイスを使用する場合は Noneを指定する。
 */
class PlayDevice(val frameRate: Double,
                 val channels: Int,
                 val theMixer: Option[Mixer.Info],
                 val theByteConverterFunc: ByteConvType) extends OutDevice {

  /**サウンドデバイスへのデータラインを取得 */
  val out = new SourceDataLineOut(mixer match {
    case Some(x) => AudioSystem.getSourceDataLine(form, x)
    case None => AudioSystem.getSourceDataLine(form)
  })

  def getAudioOut = out

  def form = new AudioFormat(frameRate.toFloat, 16, channels, true, false)

  def mixer = theMixer

  def bytesConverter = theByteConverterFunc

}

/**デフォルト(SoundSystemConstsに定義された設定)のサウンドデバイスを取得します */
object PlayDevice {
  def apply(frameRate: Double,
            channels: Int,
            theMixer: Option[Mixer.Info],
            byteConverterFunc: ByteConvType) = new PlayDevice(frameRate, channels, theMixer, byteConverterFunc)

  def default() = new PlayDevice(FRAMES_PAR_SEC, CHANNELS, None, bc16bit)
}
