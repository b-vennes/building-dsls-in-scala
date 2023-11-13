package taskdsl.v4

import taskdsl.*
import taskdsl.v4.ops.*
import cats.syntax.all.*

class CampaignBuilderTests extends munit.FunSuite:
    test("example"):
        val expected = Campaign(
            Task(
                "t1",
                taskType = TaskType.Email,
                time = TimeAt.Range(10, 12, "10-20-23")
            ),
            Task(
                "t2",
                taskType = TaskType.Call,
                time = TimeAt.Specific(10, "10-20-23")
            ),
            Task(
                "t3",
                taskType = TaskType.Mail,
                time = TimeAt.Date("10-20-27"),
                shipping = Shipping.Overnight
            )
        ).asRight

        val result = CampaignBuilder(
            email("t1")(
                within(10, 12),
                on("10-20-23")
            ),
            call("t2")(
                at(10),
                on("10-20-23")
            ),
            mail("t3")(
                by(Shipping.Overnight),
                on("10-20-27")
            )
        ).build

        assert(result === expected, s"$result was not $expected")
