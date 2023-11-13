package taskdsl

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*

import java.util.UUID

case class Task(
    id: String,
    taskType: TaskType,
    time: TimeAt,
    shipping: Shipping,
    recipients: Chain[String]
)

object Task:
    def apply(
        id: String = UUID.randomUUID().toString,
        taskType: TaskType = TaskType.None,
        time: TimeAt = TimeAt.Unspecified,
        shipping: Shipping = Shipping.None,
        recipients: Chain[String] = Chain.empty
    ): Task = new Task(id, taskType, time, shipping, recipients)

    given Eq[Task] = Eq.instance((a, b) =>
        a.id === b.id &&
            a.time === b.time &&
            a.taskType === b.taskType &&
            a.shipping === b.shipping &&
            a.recipients === b.recipients
    )

case class IntRange(from: Int, to: Int)

enum TimeAt:
    case Unspecified
    case SpecificHour(hour: Int)
    case Specific(hour: Int, date: String)
    case Date(date: String)
    case HourRange(start: Int, end: Int)
    case Range(start: Int, end: Int, date: String)
    case HoursAfterLast(hours: Int)
    case HoursAfterTaskId(id: String, hours: Int)
    case HoursAfterTask(task: Task, hours: Int)

object TimeAt:
    def daysAfter(days: Int): TimeAt.HoursAfterLast =
        TimeAt.HoursAfterLast(days * 24)

    given Eq[TimeAt] = Eq.fromUniversalEquals

    extension (time: TimeAt)
        def setDate(date: String): TimeAt =
            time match
            case TimeAt.Unspecified            => TimeAt.Date(date)
            case TimeAt.SpecificHour(hour)     => TimeAt.Specific(hour, date)
            case TimeAt.Specific(hour, date)   => TimeAt.Specific(hour, date)
            case TimeAt.Date(_)                => TimeAt.Date(date)
            case TimeAt.HourRange(start, end)  => TimeAt.Range(start, end, date)
            case TimeAt.Range(start, end, _)   => TimeAt.Range(start, end, date)
            case TimeAt.HoursAfterLast(_)      => TimeAt.Date(date)
            case TimeAt.HoursAfterTaskId(_, _) => TimeAt.Date(date)
            case TimeAt.HoursAfterTask(_, _)   => TimeAt.Date(date)

        def setRange(start: Int, end: Int): TimeAt =
            time match
            case TimeAt.Unspecified            => TimeAt.HourRange(start, end)
            case TimeAt.Specific(_, date)      => TimeAt.Range(start, end, date)
            case TimeAt.SpecificHour(_)        => TimeAt.HourRange(start, end)
            case TimeAt.Date(date)             => TimeAt.Range(start, end, date)
            case TimeAt.Range(_, _, date)      => TimeAt.Range(start, end, date)
            case TimeAt.HourRange(_, _)        => TimeAt.HourRange(start, end)
            case TimeAt.HoursAfterLast(_)      => TimeAt.HourRange(start, end)
            case TimeAt.HoursAfterTaskId(_, _) => TimeAt.HourRange(start, end)
            case TimeAt.HoursAfterTask(_, _)   => TimeAt.HourRange(start, end)

        def setHour(hour: Int): TimeAt =
            time match
            case TimeAt.Unspecified            => TimeAt.SpecificHour(hour)
            case TimeAt.SpecificHour(_)        => TimeAt.SpecificHour(hour)
            case TimeAt.Specific(_, date)      => TimeAt.Specific(hour, date)
            case TimeAt.Date(date)             => TimeAt.Specific(hour, date)
            case TimeAt.HourRange(_, _)        => TimeAt.SpecificHour(hour)
            case TimeAt.Range(_, _, date)      => TimeAt.Specific(hour, date)
            case TimeAt.HoursAfterLast(_)      => TimeAt.SpecificHour(hour)
            case TimeAt.HoursAfterTaskId(_, _) => TimeAt.SpecificHour(hour)
            case TimeAt.HoursAfterTask(_, _)   => TimeAt.SpecificHour(hour)

enum TaskType:
    case None, Email, Call, Mail

object TaskType:
    given Eq[TaskType] = Eq.fromUniversalEquals

enum Shipping:
    case None, Overnight, ThreeDay, Ground

object Shipping:
    given Eq[Shipping] = Eq.fromUniversalEquals

enum Repeat:
    case None, Daily, Weekly, Monthly

case class Campaign(tasks: Chain[Task])

object Campaign:

    case class InvalidTaskError(reason: String, taskId: String)

    object InvalidTaskError:
        given Eq[InvalidTaskError] = Eq.fromUniversalEquals

    def apply(tasks: Task*): Campaign = new Campaign(Chain.fromSeq(tasks))

    /** Match case for a task that has occurred some hours after the previous task. */
    private object hoursAfterLast:
        def unapply(
            values: Task
        ): Option[(
            String,
            TaskType,
            Int,
            Shipping,
            Chain[String]
        )] =
            values match
            case Task(id, taskType, TimeAt.HoursAfterLast(hours), shipping, recipients) =>
                (id, taskType, hours, shipping, recipients).some
            case _ => None

    private object hoursAfterTaskId:

        def unapply(
            values: Task
        ): Option[(
            String,
            TaskType,
            String,
            Int,
            Shipping,
            Chain[String]
        )] =
            values match
            case Task(id, taskType, TimeAt.HoursAfterTaskId(taskId, hours), shipping, recipients) =>
                (id, taskType, taskId, hours, shipping, recipients).some
            case _ => None

    /** Creates a campaign from the given tasks. Resolves any loose links from one task to another.
      * A failure may occur if a task is in an invalid state: linking to a non-existing task, for
      * example.
      */
    def compile(tasks: Chain[Task]): Either[InvalidTaskError, Campaign] =
        tasks.foldLeft(Chain.empty[Task].asRight[InvalidTaskError]):
            case (
                Right(tasks),
                hoursAfterLast(id, taskType, hours, shipping, recipients)
            ) =>
                tasks.lastOption
                    .toRight(InvalidTaskError(
                        "previous task doesn't exist",
                        id
                    ))
                    .map(lastTask =>
                        tasks.append(Task(
                            id,
                            taskType,
                            TimeAt.HoursAfterTask(lastTask, hours),
                            shipping,
                            recipients
                        ))
                    )
            case (
                Right(tasks),
                hoursAfterTaskId(id, taskType, afterTaskId, hours, shipping, recipients)
            ) =>
                tasks.find(_.id === afterTaskId).toRight(
                    InvalidTaskError(
                        s"Task with ID $afterTaskId not found.",
                        id
                    )
                ).map(afterTask =>
                    tasks.append(Task(
                        id,
                        taskType,
                        TimeAt.HoursAfterTask(afterTask, hours),
                        shipping,
                        recipients
                    ))
                )
            case (state, task) => state.map(_.append(task))
        .map(tasks => Campaign(tasks))

    given Eq[Campaign] = Eq.instance((a, b) =>
        a.tasks === b.tasks
    )
