package taskdsl.v3

import cats.data.Chain
import cats.syntax.all.*
import taskdsl.*

import CampaignBuilder.*

/** Improves the ergonomics of the [[v2.CampaignBuilder]] using nested builder classes. */
class CampaignBuilder(tasksContext: Chain[Task]):
    def email(id: String): TimeBuilder =
        TimeBuilder(id, TaskType.Email, tasksContext)

    def call(id: String): TimeBuilder = ???

    def mail(id: String): TimeBuilder = ???

    def build: Either[Campaign.InvalidTaskError, Campaign] = Campaign.compile(tasksContext)

object CampaignBuilder:
    def builder(): CampaignBuilder = new CampaignBuilder(Chain.empty)

    def apply(tasks: Chain[Task]): CampaignBuilder = new CampaignBuilder(tasks)
