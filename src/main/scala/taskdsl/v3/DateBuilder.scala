package taskdsl.v3

import cats.data.Chain
import taskdsl.*

trait DateBuilder:
    def on(date: String): CampaignBuilder

object DateBuilder:
    case class WithinBuilder(
        startHour: Int,
        endHour: Int,
        taskId: String,
        taskType: TaskType,
        tasksContext: Chain[Task]
    ) extends DateBuilder:
        override def on(date: String): CampaignBuilder =
            CampaignBuilder(
                tasksContext.append(Task(
                    taskId,
                    taskType,
                    TimeAt.Range(
                        startHour,
                        endHour,
                        date
                    )
                ))
            )

    case class AtBuilder(
        hour: Int,
        taskId: String,
        taskType: TaskType,
        tasksContext: Chain[Task]
    ) extends DateBuilder:
        override def on(date: String): CampaignBuilder =
            CampaignBuilder(
                tasksContext.append(Task(
                    taskId,
                    taskType,
                    TimeAt.Specific(hour, date)
                ))
            )
