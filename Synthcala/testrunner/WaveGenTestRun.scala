import jp.noisyspot.synth.gen.WaveGenerator._
import jp.noisyspot.synth.out.{OutDevice, DefaultOutDevice}
import jp.noisyspot.synth.SoundSystemConsts._

object WaveGenTestRun {
  def main(args: Array[String]) {
    val sqConc = squareWave(FRAMES_PAR_SEC, 0.8, 440.0) andThen vol(0.2) andThen pan(0.5)
    val sineConc = sineWave(FRAMES_PAR_SEC, 440.0) andThen vol(0.5) andThen pan(0.2)
    val triConc = triWave(FRAMES_PAR_SEC, 110.0) andThen vol(0.2) andThen pan(0.8)
    val end = FRAMES_PAR_SEC.toInt * 2

    val device = DefaultOutDevice()
    device.start()
    device ! Range(0, end).flatMap(sqConc)
    device ! Range(0, end).flatMap(sineConc)
    device ! Range(0, end).flatMap(triConc)
    device !? OutDevice.End
    ()
  }
}
