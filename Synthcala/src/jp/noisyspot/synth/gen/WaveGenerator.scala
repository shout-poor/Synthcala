package jp.noisyspot.synth.gen

/**
 * 波形生成関数
 */
object WaveGenerator {

  def pan(panpot: Double) = (x: Double) => {
    require(panpot >= 0.0 && panpot <= 1.0)
    List(x * (1.0 - panpot), x * panpot)
  }

  def squareWave(sampleRate: Double, dutyRate: Double, volume: Double, tone: Double) = (z: Int) => {
    val waveLen = sampleRate / tone
    if (z.toDouble % waveLen <= waveLen * dutyRate) volume else -volume
  }

  def sineWave(sampleRate: Double, volume: Double, tone: Double) = (z: Int) => {
    import scala.math._
    val n = z.toDouble / (sampleRate / tone)
    sin((n - n.floor) * 2.0 * Pi)
  }

}