package taskdsl.v4

import cats.data.Chain
import taskdsl.*

/**
 * Contains operations to build a set of Campaign tasks in a structured style.
 */
object ops:

    /**
     * Defines a modification to a time property of a task.
     */
    enum TimeModifier:
        case Within(start: Int, end: Int)
        case On(date: String)
        case At(hour: Int)

    enum MailModifier:
        case By(shippingMethod: Shipping)

    def within(start: Int, end: Int): TimeModifier =
        TimeModifier.Within(start, end)

    def on(date: String): TimeModifier =
        TimeModifier.On(date)

    def at(hour: Int): TimeModifier =
        TimeModifier.At(hour)

    def by(shipping: Shipping): MailModifier =
        MailModifier.By(shipping)

    private def compileTask(id: String, taskType: TaskType, props: Chain[TimeModifier | MailModifier]): Task =
        props.foldLeft(Task(id = id, taskType = taskType)):
            case (task, TimeModifier.Within(start, end)) =>
                task.copy(time = task.time.setRange(start, end))
            case (task, TimeModifier.On(date)) =>
                task.copy(time = task.time.setDate(date))
            case (task, TimeModifier.At(hour: Int)) => task.copy(time = task.time.setHour(hour))
            case (task, MailModifier.By(shipping)) =>
                task.copy(shipping = shipping)

    def email(id: String)(taskProps: TimeModifier*): Task =
        compileTask(id, TaskType.Email, Chain.fromSeq(taskProps))

    def call(id: String)(taskProps: TimeModifier*): Task =
        compileTask(id, TaskType.Call, Chain.fromSeq(taskProps))

    /**
     * Defines a mail campaign task.  Allows modifier to both time and mail-specific properties.
     */
    def mail(id: String)(taskProps: (TimeModifier | MailModifier)*): Task =
        compileTask(id, TaskType.Mail, Chain.fromSeq(taskProps))
