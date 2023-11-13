package taskdsl.v5

import cats.data.Chain
import taskdsl.*

/**
 * Contains operations to build a set of Campaign tasks in a structured style.
 *
 * Adds some new ops to v4's set of operations:
 * `id` assignment, "hours after task", and added recipients for email syntax.
 */
object ops:

    /**
     * Defines a modification to a time property of a task.
     */
    enum TimeModifier:
        case Within(start: Int, end: Int)
        case On(date: String)
        case At(hour: Int)
        case After(taskId: String, hours: Int)

    enum ShippingModifier:
        case By(shippingMethod: Shipping)

    enum RecipientModifier:
        case Add(recipient: String)

    enum TaskModifier:
        case Name(id: String)

    type MailModifier = TimeModifier | ShippingModifier | TaskModifier

    type EmailModifier = TimeModifier | RecipientModifier | TaskModifier

    type CallModifier = TimeModifier | TaskModifier

    def within(start: Int, end: Int): TimeModifier =
        TimeModifier.Within(start, end)

    def on(date: String): TimeModifier =
        TimeModifier.On(date)

    def at(hour: Int): TimeModifier =
        TimeModifier.At(hour)

    def by(shipping: Shipping): ShippingModifier =
        ShippingModifier.By(shipping)

    val morning: TimeModifier =
        TimeModifier.Within(6, 12)

    /**
     * Some syntactic trickery to make the DSL `id := "exampleId"` work.
     */
    class NameModifierBuilder:
        def :=(id: String): TaskModifier =
            TaskModifier.Name(id)

    val id = new NameModifierBuilder

    extension (s: String)
        def `unary_+`: RecipientModifier = RecipientModifier.Add(s)

        def `+=`(hours: Int): TimeModifier = TimeModifier.After(s, hours)

    /**
     * Creates a task using the given defaults and set of modifications.
     */
    private def compileTask(id: String, taskType: TaskType, mods: Chain[TimeModifier | ShippingModifier | RecipientModifier | TaskModifier]): Task =
        mods.foldLeft(Task(id = id, taskType = taskType)):
            case (task, TimeModifier.Within(start, end)) =>
                task.copy(time = task.time.setRange(start, end))
            case (task, TimeModifier.On(date)) =>
                task.copy(time = task.time.setDate(date))
            case (task, TimeModifier.At(hour: Int)) => task.copy(time = task.time.setHour(hour))
            case (task, ShippingModifier.By(shipping)) =>
                task.copy(shipping = shipping)
            case (task, RecipientModifier.Add(recipient)) =>
                task.copy(recipients = task.recipients.append(recipient))
            case (task, TimeModifier.After(id, hours)) =>
                task.copy(time = TimeAt.HoursAfterTaskId(id, hours))
            case (task, TaskModifier.Name(id)) =>
                task.copy(id = id)

    def email(id: String)(taskProps: EmailModifier*): Task =
        compileTask(id, TaskType.Email, Chain.fromSeq(taskProps))

    def email(mods: EmailModifier*): Task = email("")(mods: _*)

    def call(id: String)(taskProps: CallModifier*): Task =
        compileTask(id, TaskType.Call, Chain.fromSeq(taskProps))

    def call(mods: CallModifier*): Task = call("")(mods: _*)

    /**
     * Defines a mail campaign task.  Allows modifier to both time and mail-specific properties.
     */
    def mail(id: String)(taskProps: MailModifier*): Task =
        compileTask(id, TaskType.Mail, Chain.fromSeq(taskProps))

    def mail(mods: MailModifier*): Task = mail("")(mods: _*)
