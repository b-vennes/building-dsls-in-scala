package taskdsl.v2

import taskdsl.*
import cats.syntax.all.*

class CampaignBuilderTests extends munit.FunSuite:
    test("example"):
        val task1 = Task(
            id = "task1",
            taskType = TaskType.Call,
            time = TimeAt.Specific(1, "11-30-20")
        )
        val expected = Campaign(
            task1,
            Task(
                id = "task2",
                taskType = TaskType.Call,
                time = TimeAt.HoursAfterTask(task1, 10)
            )
        ).asRight

        val result = CampaignBuilder.builder()
            .call("task1", 1, "11-30-20")
            .callAfter("task2", 10)
            .build()

        assert(result === expected, s"$result is not $expected")
