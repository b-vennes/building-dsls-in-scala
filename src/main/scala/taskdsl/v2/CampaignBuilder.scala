package taskdsl.v2

import cats.data.Chain
import taskdsl.*

/** Same as the [[v1.CampaignBuilder]], but supports a linked task using the [[callAfter]] method.
  */
class CampaignBuilder(taskOrder: Chain[Task]):
    def emailWithin(
        id: String,
        date: String,
        start: Int = -1,
        end: Int = -1
    ): CampaignBuilder =
        add(Task(id, TaskType.Email, TimeAt.Range(start, end, date)))

    def call(id: String, hour: Int, date: String): CampaignBuilder =
        add(Task(id, TaskType.Call, TimeAt.Specific(hour, date)))

    def callAfter(id: String, hours: Int): CampaignBuilder =
        add(Task(id, TaskType.Call, TimeAt.HoursAfterLast(hours)))

    def mail(id: String, shipping: Shipping, date: String): CampaignBuilder =
        add(Task(id, TaskType.Call, TimeAt.Date(date), shipping))

    def build(): Either[Campaign.InvalidTaskError, Campaign] = Campaign.compile(taskOrder)

    private def add(task: Task): CampaignBuilder =
        CampaignBuilder(taskOrder.append(task))

object CampaignBuilder:
    def builder(): CampaignBuilder = new CampaignBuilder(Chain.empty)
