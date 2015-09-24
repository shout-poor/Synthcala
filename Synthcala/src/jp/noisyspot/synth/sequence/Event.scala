package jp.noisyspot.synth.sequence

/**
 * イベントの譜面上のタイミング
 */
case class EventTime(bar: Int, clock: Int)

/**
 * イベントtrait
 */
sealed trait Event {
  def time: EventTime
}

/** Start of sequence */
case class StartEvent() extends Event {
  def time = EventTime(0, 0)
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

