package taskdsl.v3

import taskdsl.*

import cats.syntax.all.*

class CampaignBuilderTests extends munit.FunSuite:
    test("example"):
        val expected = Campaign(
            Task(
                id = "t1",
                time = TimeAt.Range(10, 12, "10-23-23"),
                taskType = TaskType.Email
            )
        ).asRight

        val result = CampaignBuilder.builder()
            .email("t1")
            .within(10, 12)
            .on("10-23-23")
            .build

        assert(result === expected)
