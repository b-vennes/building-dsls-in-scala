package taskdsl.v4

import cats.data.Chain
import taskdsl.*

/** A re-organization of steps available in [[taskdsl.v3.CampaignBuilder]]
 * which uses declarative task construction defined in the [[ops]] module. */
class CampaignBuilder(tasks: Chain[Task]):
    def build: Either[Campaign.InvalidTaskError, Campaign] = Campaign.compile(tasks)

object CampaignBuilder:

    def apply(
        tasks: Task*
    ): CampaignBuilder = new CampaignBuilder(Chain.fromSeq(tasks))
