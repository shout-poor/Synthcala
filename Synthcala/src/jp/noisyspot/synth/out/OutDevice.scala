package jp.noisyspot.synth.out

import javax.sound.sampled._
import actors.Actor
import scala.Int

trait AudioOut {
  def write(b: Array[Byte], start: Int, length: Int): Int
  def open(f: AudioFormat)
  def start()
  def flush()
  def stop()
  def close()
}

trait OutDevice extends Actor {

  def form: AudioFormat
  def mixer: Option[Mixer.Info]
  def bytesConverter: ByteConverterFuncs.ByteConvType
  def getAudioOut: AudioOut

  /**
   * 出力処理スレッド。
   * Seq[Double]を受信した場合は、Byte配列サウンドデバイスへ出力する
   * OutDevice.Flushを受診した場合は、データラインをflushする。
   * OutDevice.Endを受診した場合は、戻り値"OK"を返しスレッドを終了する。
   */
  def act() {
    val line = getAudioOut
    line.open(form)
    line.start()
    loop {
      react {
        case frame: Seq[Double] => output(frame, line)
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
  def output(samples: Seq[Double], line: AudioOut)  {

    val l = samples.map((sample) => sample match {
      // サンプルを -1.0 ～ 1.0 の範囲にクリッピング
      case x if (x > 1.0) => 1.0
      case x if (x < -1.0) => -1.0
      case x => x
    }).flatMap(bytesConverter)

    line.write(l.toArray, 0, l.length)
  }
}

object ByteConverterFuncs {
  type ByteConvType = (Double) => Seq[Byte]
  val bc16bit: ByteConvType = (sample: Double) => {
    // 符号付き16bitのバイト列に変換
    val intVal = (sample * (if (sample < 0.0) 32768.0 else 32767.0)).toInt
    List((intVal & 0xff).toByte, (intVal >> 8).toByte)
  }
}

/**OutDeviceのコンパニオンオブジェクト。メッセージの型を定義します。 */
object OutDevice {

  /**オーディオデバイスのDataLineにバッファフラッシュさせるメッセージ */
  object Flush {}

  /**OutDevice のスレッドを終了させるメッセージ */
  object End {}

}

