package taskdsl.v3

trait TaskBuilder:
    def within(startHour: Int, endHour: Int): DateBuilder

    def at(hour: Int): DateBuilder
