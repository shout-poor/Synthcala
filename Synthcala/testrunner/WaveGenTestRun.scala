import jp.noisyspot.synth.gen.WaveGenerator._
import jp.noisyspot.synth.out.{OutDevice, DefaultOutDevice}
import jp.noisyspot.synth.SoundSystemConsts._

object WaveGenTestRun {
  def main(args: Array[String]) {

    // デューティ比0.8、音程440Hz、音量0.2、定位0.5 (中央) の矩形波関数
    val sqConc = squareWave(FRAMES_PAR_SEC, 0.8, 440.0) andThen vol(0.2) andThen pan(0.5)

    // 音程440Hz、音量0.5、定位0.2 の矩形波関数
    val sineConc = sineWave(FRAMES_PAR_SEC, 440.0) andThen vol(0.5) andThen pan(0.2)

    // 音程110Hz、音量0.2、定位0.8 の三角波関数
    val triConc = triWave(FRAMES_PAR_SEC, 110.0) andThen vol(0.2) andThen pan(0.8)

    // 発声時間（2秒)
    val end = 2 * FRAMES_PAR_SEC.toInt

    // サウンドデバイス
    val device = DefaultOutDevice()

    device.start()
    device ! Range(0, end).flatMap(sqConc)
    device ! Range(0, end).flatMap(sineConc)
    device ! Range(0, end).flatMap(triConc)
    device !? OutDevice.End
    ()
  }
}
