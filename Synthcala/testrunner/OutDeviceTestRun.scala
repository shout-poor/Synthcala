import jp.noisyspot.synth.out.{OutDevice, DefaultOutDevice}
import jp.noisyspot.synth.SoundSystemConsts._
import reflect.Literal

object OutDeviceTestRun {
  def main(args: Array[String]) {

    val TONE = 440.0
    val HALF_WAVE_LENGTH = 0.5 / TONE
    val DT = 1.0 / FRAMES_PAR_SEC
    val OUT_TIME_SEC = 2.0

    val device = DefaultOutDevice()
    device.start()

    def waveIter(t: Double, p: Double, level: Double, cont: List[Double]): List[Double] = {
      (t + DT, p + DT) match {
        case (nt, np) if (nt >= OUT_TIME_SEC)     => cont.reverse
        case (nt, np) if (np >= HALF_WAVE_LENGTH) => waveIter(nt, np - HALF_WAVE_LENGTH, -level, level :: cont)
        case (nt, np)                             => waveIter(nt, np, level, level :: cont)
      }
    }
    device ! waveIter(0.0, 0.0, 0.8, List.empty).flatMap((x) => List(x, 0.0))  // 左だけ
    device ! waveIter(0.0, 0.0, 0.8, List.empty).flatMap((x) => List(0.0, x))  // 右だけ
    device ! waveIter(0.0, 0.0, 0.8, List.empty).flatMap((x) => List(x, x))    // センター
    device !? OutDevice.End
    ()
  }
}
