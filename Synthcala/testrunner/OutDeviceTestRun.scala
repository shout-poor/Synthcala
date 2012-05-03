import jp.noisyspot.synth.out.{OutDevice, DefaultOutDevice}
import jp.noisyspot.synth.SoundSystemConsts._

object OutDeviceTestRun {
  def main(args: Array[String]): Unit = {

    val TONE = 440.0
    val HALF_WAVE_LENGTH = 0.5 / TONE
    val DT = 1.0 / FRAMES_PAR_SEC
    val OUT_TIME_SEC = 2.0

    val device = DefaultOutDevice()
    device.start

    def waveIter(t: Double, p: Double, level: Double, cont: List[Double]): List[Double] = {
      (t + DT, p + DT) match {
        case (nt, np) if (nt >= OUT_TIME_SEC)     => cont.reverse
        case (nt, np) if (np >= HALF_WAVE_LENGTH) => waveIter(nt, np - HALF_WAVE_LENGTH, -level, level :: level :: cont)
        case (nt, np)                             => waveIter(nt, np, level, level :: level :: cont)
      }
    }
    device ! waveIter(0.0, 0.0, 1.0, List.empty)
    device !? OutDevice.End
    ()
  }
}
