package jp.noisyspot.synth.gen

/**
 * 波形生成関数
 */
object WaveGenerator {

  /** 定位を0.0～1.0の範囲で設定。0.0が左端、0.5が中央、1.0が右端 */
  def pan(panpot: Double) = (x: Double) => List(x * (1.0 - panpot), x * panpot)

  /** ボリュームを 0.0 ～ 1.0 の範囲で指定 */
  def vol(volume: Double) = (x: Double) => x * volume

  /**
   * 矩形波関数を取得する
   *
   * @param sampleRate サンプリングレート
   * @param dutyRate デューティ比 (0.0 ～ 1.0)
   * @param tone 音程（周波数Hz)
   * @return 時間変数zを引数とする矩形波関数
   */
  def squareWave(sampleRate: Double, dutyRate: Double, tone: Double) = (z: Int) => {
    if (pInWave(sampleRate, tone, z) <= dutyRate) 1.0 else -1.0
  }

  /**
   * 正弦波関数を取得する
   *
   * @param sampleRate サンプリングレート
   * @param tone 音程（周波数Hz)
   * @return 時間変数zを引数とする正弦波関数
   */
  def sineWave(sampleRate: Double, tone: Double) = (z: Int) => {
    import scala.math._
    sin(pInWave(sampleRate, tone, z) * 2.0 * Pi)
  }

  /**
   * 三角波関数を取得する
   *
   * @param sampleRate サンプリングレート
   * @param tone 音程（周波数Hz)
   * @return 時間変数zを引数とする三角波関数
   */
  def triWave(sampleRate: Double, tone: Double) = (z: Int) => {
    val dt = 4.0 / waveLen(sampleRate, tone)

    pInWave(sampleRate, tone, z) match {
      case p if (p < .25) => p * dt
      case p if (p >= .25 && p < .75) => 1.0 - (p - .25) * dt
      case p => -1.0 + (p - .75) * dt
    }
  }

  /**サンプリングレートと周波数から、サンプル単位の波長を取得 */
  def waveLen(sampleRate: Double, tone: Double) = sampleRate / tone

  /**一波形の中の現在位置を0.0 ～ 1.0の範囲で取得 */
  def pInWave(sampleRate: Double, tone: Double, z: Int) = {
    val n = z.toDouble / waveLen(sampleRate, tone)
    n - n.floor
  }

}