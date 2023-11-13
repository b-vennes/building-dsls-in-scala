package taskdsl.v1

import cats.data.Chain
import taskdsl.*

/** A very simple [[Campaign]] builder with method definitions for each supported task case. No
  * linking between tasks is allowed.
  */
class CampaignBuilder(taskOrder: Chain[Task]):

    def emailWithin(
        id: String,
        start: Int,
        end: Int,
        date: String
    ): CampaignBuilder =
        add(Task(id, TaskType.Email, TimeAt.Range(start, end, date)))

    def call(id: String, hour: Int, date: String): CampaignBuilder =
        add(Task(id, TaskType.Call, TimeAt.Specific(hour, date)))

    def mail(id: String, shipping: Shipping, date: String): CampaignBuilder =
        add(Task(id, TaskType.Mail, TimeAt.Date(date), shipping))

    def build(): Either[Campaign.InvalidTaskError, Campaign] =
        Campaign.compile(taskOrder)

    private def add(task: Task): CampaignBuilder =
        CampaignBuilder(taskOrder.append(task))

object CampaignBuilder:
    def builder(): CampaignBuilder = new CampaignBuilder(Chain.empty)
