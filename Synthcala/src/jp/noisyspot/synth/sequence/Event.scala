package jp.noisyspot.synth.sequence

case class EventTime(bar: Int, clock: Int) {
  def gap(b: EventTime, clocksPerBar: Int) = (b.bar - this.bar) * clocksPerBar + (b.clock - this.clock)
}

sealed trait Event {
  def time: EventTime
}
case class NoteOn(time: EventTime, noteNo: Int, velocity: Int) extends Event
case class NoteOff(time: EventTime, noteNo: Int) extends Event
case class TempoChange(time: EventTime, tempo: Int) extends Event
case class VolumeChange(time: EventTime, volume: Int) extends Event
case class BeatChange(time: EventTime, numerator: Int, denominator: Int) extends Event

