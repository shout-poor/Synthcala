package jp.noisyspot.synth.sequence

/**
 * イベントの譜面上のタイミング
 * @param bar   小節
 * @param clock 小節内のクロック（クロック解像度は四分音符あたり48とする）
 */
case class EventTime(bar: Int, clock: Int) {
  def gap(b: EventTime, clocksPerBar: Int) = (b.bar - this.bar) * clocksPerBar + (b.clock - this.clock)
}

/**
 * イベントtrait
 */
sealed trait Event {
  def time: EventTime
}

/** Note on event*/
case class NoteOn(time: EventTime, noteNo: Int, velocity: Int) extends Event
/** Note off event */
case class NoteOff(time: EventTime, noteNo: Int) extends Event
/** change tempo event */
case class TempoChange(time: EventTime, tempo: Int) extends Event
/** change volume of track */
case class VolumeChange(time: EventTime, volume: Int) extends Event
/** change beat event */
case class BeatChange(time: EventTime, numerator: Int, denominator: Int) extends Event

