import jp.noisyspot.synth.gen.WaveGenerator._
import jp.noisyspot.synth.out.{OutDevice, DefaultOutDevice}
import jp.noisyspot.synth.SoundSystemConsts._

object WaveGenTestRun {
  def main(args: Array[String]) {
    val sqStream = toStreamMono(squareWave(FRAMES_PAR_SEC, .5, .5, 440.0, _: BigInt))
    val sineStream = toStreamMono(sineWave(FRAMES_PAR_SEC, .5, 440.0, _: BigInt))

    val device = DefaultOutDevice()
    device.start()
    device ! sqStream(0, FRAMES_PAR_SEC.toInt * 2).flatMap((x) => List(x, x))
    device ! sineStream(0, FRAMES_PAR_SEC.toInt * 2).flatMap((x) => List(x, x))
    device !? OutDevice.End
    ()
  }
}
