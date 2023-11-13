package taskdsl.v3

import cats.data.Chain
import taskdsl.{Task, TaskType}

case class TimeBuilder(
    id: String,
    taskType: TaskType,
    tasksContext: Chain[Task]
) extends TaskBuilder:
    self =>

    import TimeBuilder.*

    def within(startHour: Int, endHour: Int): DateBuilder =
        DateBuilder.WithinBuilder(startHour, endHour, id, taskType, tasksContext)

    def at(hour: Int): DateBuilder =
        DateBuilder.AtBuilder(hour, id, taskType, tasksContext)
case class MailBuilder(id: String, campaignBuilder: CampaignBuilder) {}

object MailBuilder {}
