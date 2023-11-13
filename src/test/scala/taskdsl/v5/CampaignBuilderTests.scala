package taskdsl.v5

import cats.data.Chain
import cats.syntax.all.*
import taskdsl.*
import taskdsl.v4.CampaignBuilder
import taskdsl.v5.ops.*

class CampaignBuilderTests extends munit.FunSuite:
    test("example"):
        val task1 = Task(
            "t1",
            taskType = TaskType.Email,
            time = TimeAt.Range(6, 12, "10-20-23"),
            recipients = Chain(
                "test@gmail.com",
                "hi@hello.net"
            )
        )
        val expected = Campaign(
            task1,
            Task(
                "t2",
                taskType = TaskType.Call,
                time = TimeAt.HoursAfterTask(task1, 5)
            ),
            Task(
                "t3",
                taskType = TaskType.Mail,
                time = TimeAt.Date("10-20-27"),
                shipping = Shipping.Overnight
            )
        ).asRight

        val result = CampaignBuilder(
            email(
                id := "t1",
                morning,
                on("10-20-23"),
                +"test@gmail.com",
                +"hi@hello.net",
            ),
            call(
                id := "t2",
                "t1" += 5,
            ),
            mail(
                id := "t3",
                by(Shipping.Overnight),
                on("10-20-27"),
            )
        ).build

        assert(result === expected, s"$result was not $expected")
