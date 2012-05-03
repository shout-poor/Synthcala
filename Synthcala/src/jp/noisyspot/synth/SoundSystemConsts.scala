package jp.noisyspot.synth

/**
 * システム定数（デフォルトのオーディオフォーマット等）
 */

object SoundSystemConsts {
  val FRAMES_PAR_SEC = 44100.0
  val CHANNELS = 2
  val SAMPLES_PAR_SEC = FRAMES_PAR_SEC * CHANNELS
  val SAMPLE_SIZE_IN_BITS = 16
  val FRAME_SIZE = SAMPLE_SIZE_IN_BITS / 8 * CHANNELS
}
