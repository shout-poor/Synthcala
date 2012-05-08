package jp.noisyspot.synth.sequence

/**
 * 実時間保持クラス
 * @param sec   秒数
 * @param frame 秒内のフレーム。1フレームは (1/サンプリングレート)秒
 */
case class RealTime(sec: Int, frame: Int)

/**
 * イベントリスト内の演奏状態保持クラス
 * @param tempo         テンポ
 * @param clocksPerBar  小節あたりの総クロック数（拍子によって変化する）
 * @param trackVolume   トラック音量
 */
case class SectionState(tempo: Int, clocksPerBar: Int, trackVolume: Int) {
  def tempoChange(newTempo:Int) = SectionState(newTempo, clocksPerBar, trackVolume)
  def volumeChange(newVolume:Int) = SectionState(tempo, clocksPerBar, newVolume)
  def beatChange(numerator:Int, denominator: Int) =
    SectionState(tempo, 192 * numerator / denominator, trackVolume)

  def framesPerClock(sampleRate: Double) = sampleRate * tempo.toDouble * .8 // sampleRate / ((60.0 / tempo) / 48.0)

}

/**
 * イベントリストクラス。Seq[Event]とサンプリングレートを保持し、実時間にマップした情報を返す
 * @param list        イベントの一覧
 * @param sampleRate  サンプリングレート
 */
class EventList(val list: Seq[Event], val sampleRate: Double) {
  val INITIAL_STATE = SectionState(120, 192, 100) // テンポ、小節あたりクロック数、音量 の初期値

  /** 実時間にマップされたイベントリストを返す */
  def eventMap = {
    def iter(curList: Seq[Event], state: SectionState, cont: List[(RealTime, Event)]) : Map[RealTime, Event] = {
      curList match {
        case h :: t => {
          val (preTime, preEvent) = cont.head
          val n = (h.time.gap(preEvent.time, state.clocksPerBar) * state.framesPerClock(sampleRate)).round.toInt +
            preTime.frame
          val next = (RealTime(preTime.sec + n / sampleRate.toInt, n % sampleRate.toInt), h)
          val nextState = h match {
            case TempoChange(_, tempo) => state.tempoChange(tempo)
            case VolumeChange(_, volume) => state.volumeChange(volume)
            case BeatChange(_, num, denom) => state.beatChange(num, denom)
            case _ => state
          }
          iter(t, nextState, next::cont)
        }
        case Nil => cont.toMap
      }
    }

    iter(list, INITIAL_STATE, List.empty)
  }

}
