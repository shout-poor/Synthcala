package jp.noisyspot.synth.gen

/**
 * 波形生成関数
 */
object WaveGenerator {

  def toStreamMono(f: (BigInt) => Double): (BigInt, BigInt) => Stream[Double] = {(from, to) => {
    def iter(z: BigInt): Stream[Double] = {
      if (z > to) Stream.empty else f(z) #:: iter(z + 1)
    }
    iter(from)
  }}

  def squareWave(sampleRate: Double, dutyRate: Double, volume: Double, tone: Double, z: BigInt) : Double = {
    val waveLen = sampleRate / tone
    if (z.toDouble % waveLen <= waveLen * dutyRate) volume else -volume
  }

  def sineWave(sampleRate: Double, volume: Double, tone: Double, z: BigInt) : Double = {
    import scala.math._
    val n = z.toDouble / (sampleRate / tone)
    sin((n - n.floor) * 2.0 * Pi)
  }
}