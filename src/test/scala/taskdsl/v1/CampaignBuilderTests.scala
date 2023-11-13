package taskdsl.v1

import taskdsl.*

import cats.syntax.all.*

class CampaignBuilderTests extends munit.FunSuite:
    test("example"):
        val expected = Campaign(
            Task(
                id = "task1",
                taskType = TaskType.Call,
                time = TimeAt.Specific(1, "11-22-23")
            ),
            Task(
                id = "task2",
                taskType = TaskType.Email,
                time = TimeAt.Range(9, 14, "09-20-17")
            ),
            Task(
                id = "task3",
                taskType = TaskType.Mail,
                time = TimeAt.Date("03-17-24"),
                shipping = Shipping.Overnight
            )
        ).asRight

        val result = CampaignBuilder.builder()
            .call("task1", 1, "11-22-23")
            .emailWithin("task2", 9, 14, "09-20-17")
            .mail("task3", Shipping.Overnight, "03-17-24")
            .build()

        assert(result === expected, s"$result not equal to $expected")
