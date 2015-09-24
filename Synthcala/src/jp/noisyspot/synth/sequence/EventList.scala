package jp.noisyspot.synth.sequence

/**
 * 実時間保持クラス
 * @param sec   秒数
 * @param frame 秒内のフレーム。1フレームは (1/サンプリングレート)秒
 */
case class RealTime(sec: Long, frame: Long, sampleRate:Double) {
  def newRealTime(sec: Long, frame: Long) = RealTime(sec, frame, sampleRate)

  def advance(advFrames: Long) =
    newRealTime(sec + (advFrames / sampleRate).toLong, frame + (advFrames % sampleRate.toLong))
}
object RealTime {
  def start(sampleRate: Double) = RealTime(0, 0, sampleRate)
}

/**
 * イベントリスト内の演奏状態保持クラス
 * @param time            実時刻
 * @param tempo          テンポ
 * @param clocksPerBar  小節あたりの総クロック数（拍子によって変化する）
 * @param trackVolume   トラック音量
 */
case class SectionState(time: RealTime, tempo: Int, clocksPerBar: Int, trackVolume: Int) {

  /**
   * イベントのtime(小節＋クロック)から実時刻を算出し、テンポ等の状態を更新させる。
   * @param prevEvent ひとつ前のイベント
   * @param nextEvent 新しいイベント
   * @return 更新された状態
   */
  def next(prevEvent: Event, nextEvent: Event): SectionState = {
    val frameGap: Long = framesPerClock(time.sampleRate).round *
      (nextEvent.time.bar - prevEvent.time.bar) * clocksPerBar + (nextEvent.time.clock - prevEvent.time.clock)
    val nextTime = time.advance(frameGap)
    nextEvent match {
      case TempoChange(_, nextTempo) => tempoChange(nextTime, nextTempo)
      case VolumeChange(_, volume) => volumeChange(nextTime, volume)
      case BeatChange(_, num, denom) => beatChange(nextTime, num, denom)
      case _ => updateTime(nextTime)
    }
  }

  /**
   * テンポを変更する
   * @param nextTime 実時刻
   * @param newTempo 変更後のテンポ
   * @return テンポが変更されたSectionState
   */
  def tempoChange(nextTime:RealTime, newTempo:Int) = SectionState(nextTime, newTempo, clocksPerBar, trackVolume)

  /**
   * トラック音量を変更する
   * @param nextTime 実時刻
   * @param newVolume 変更後の音量
   * @return 音量が変更されたSectionState
   */
  def volumeChange(nextTime:RealTime, newVolume:Int) = SectionState(nextTime, tempo, clocksPerBar, newVolume)

  /**
   * 拍子を変更する
   * @param nextTime 実時刻
   * @param numerator   変更後の拍子（分子）
   * @param denominator 変更後の拍子（分母）
   * @return 拍子が変更されたSectionState
   */
  def beatChange(nextTime:RealTime, numerator:Int, denominator: Int) =
    SectionState(nextTime, tempo, 192 * numerator / denominator, trackVolume)

  /**
   * 時刻のみ更新する。
   * @param nextTime 実時刻
   * @return 実時刻のみ更新されたSectionState
   */
  def updateTime(nextTime: RealTime) = SectionState(nextTime, tempo, clocksPerBar, trackVolume)

  /**
   * サンプルレートと演奏状態から、1クロック(192分音符)のフレーム数を算出する
   * @param sampleRate サンプルレート
   * @return
   */
  // sampleRate / ((60.0 / tempo) / 48.0)
  def framesPerClock(sampleRate: Double) = sampleRate * tempo.toDouble * .8

}

/**
 * イベントリストクラス。Seq[Event]とサンプリングレートを保持し、実時間にマップした情報を返す
 * @param list        イベントの一覧
 * @param sampleRate  サンプリングレート
 */
class EventList(val list: Seq[Event], val sampleRate: Double) {
  val INITIAL_STATE = SectionState(RealTime.start(sampleRate), 120, 192, 100) // テンポ、小節あたりクロック数、音量 の初期値

  /** 実時間にマップされたイベントリストを返す */
  def timeMappedEvents: Seq[(RealTime, Event)] =
    list.foldLeft(List[(SectionState, Event)]())((cont: List[(SectionState, Event)], current: Event) => {
      val (prevState, prevEvent) = cont.headOption.getOrElse((INITIAL_STATE, StartEvent()))
      (prevState.next(prevEvent, current), current)::cont
    }).map((x) => (x._1.time, x._2))
      .reverse
}
