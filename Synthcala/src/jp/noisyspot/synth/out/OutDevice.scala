package jp.noisyspot.synth.out

import jp.noisyspot.synth.SoundSystemConsts._
import javax.sound.sampled._
import actors.Actor
import scala.Int

/**
 * 出力サウンドデバイスの処理trait。
 */
trait OutDevice extends Actor {

  def form: AudioFormat
  def mixer: Option[Mixer.Info]
  def bytesConverter: (Double) => Seq[Byte]

  /**サウンドデバイスへのデータラインを取得 */
  val line = mixer match {
    case Some(x) => AudioSystem.getSourceDataLine(form, x)
    case None => AudioSystem.getSourceDataLine(form)
  }

  /**
   * 出力処理スレッド。
   * Seq[Double]を受信した場合は、Byte配列サウンドデバイスへ出力する
   * OutDevice.Flushを受診した場合は、データラインをflushする。
   * OutDevice.Endを受診した場合は、戻り値"OK"を返しスレッドを終了する。
   */
  def act() {
    line.open(form)
    line.start()
    loop {
      react {
        case frame: Seq[Double] => output(frame)
        case OutDevice.Flush => line.flush()
        case OutDevice.End => {
          line.stop()
          line.close()
          reply("OK")
          exit()
        }
      }
    }
  }

  /**
   * 受信した Double 型サンプル列をバイト列に変換し、サウンドデバイスのデータラインに出力する。
   * @param samples サンプル列。ステレオ(channels=2)の場合は、L, R, L, R…の順に並んでいること
   */
  def output(samples: Seq[Double]) = {

    val l = samples.map((sample) => sample match {
      // サンプルを -1.0 ～ 1.0 の範囲にクリッピング
      case x if (x > 1.0) => 1.0
      case x if (x < -1.0) => -1.0
      case x => x
    }).flatMap(bytesConverter);

    line.write(l.toArray, 0, l.length)
  }
}

/**
 * 16bitサウンドデバイス用のOutDevice実装クラス
 * @param frameRate フレームレート
 * @param channels チャネル数（モノラル：1 / ステレオ：2)
 * @param theMixer 使用デバイスを指定するMixer.infoオブジェクト。デフォルトデバイスを使用する場合は Noneを指定する。
 */
class OutDevice16bit(val frameRate: Double,
                     val channels: Int,
                     val theMixer: Option[Mixer.Info]) extends OutDevice {

  override def form = new AudioFormat(frameRate.toFloat, 16, channels, true, false)
  override def bytesConverter = (sample: Double) => {
    // 符号付き16bitのバイト列に変換
    val intVal = (sample * (if (sample < 0.0) 32768.0 else 32767.0)).toInt
    List(
      (intVal & 0xff).toByte,
      (intVal >> 8).toByte
    )
  }
  override def mixer = theMixer
}

/**OutDeviceのコンパニオンオブジェクト。また、メッセージの型を定義します。 */
object OutDevice {

  /**オーディオデバイスのDataLineにバッファフラッシュさせるメッセージ */
  object Flush {}

  /**OutDevice のスレッドを終了させるメッセージ */
  object End {}

}

/**デフォルト(SoundSystemConstsに定義された設定)のサウンドデバイスを取得します */
object DefaultOutDevice {
  def apply() = new OutDevice16bit(FRAMES_PAR_SEC, CHANNELS, None);
}
