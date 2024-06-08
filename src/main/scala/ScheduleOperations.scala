import java.io.{BufferedWriter, File, FileWriter, IOException}
import scala.io.Source
import scala.util.matching.Regex
import UniversityManager._

object ScheduleOperations {
  var schedule: List[Schedule] = List()

  def getFreeOptions(day: String, time: String, occupied: List[String], allOptions: List[String]): List[String] = {
    allOptions.filterNot(occupied.contains)
  }

  // Проверка занятости
  def isAvailableScheduleSlot(day: String, time: String, teacher: String, group: String, auditorium: String): Map[String, Option[String]] = {
    val errors = scala.collection.mutable.Map[String, Option[String]]()

    if (schedule.exists(entry => entry.day == day && entry.time == time && entry.auditorium == auditorium)) {
      errors("auditorium") = Some(s"Аудитория $auditorium занята в это время.")
    } else {
      errors("auditorium") = None
    }

    if (schedule.exists(entry => entry.day == day && entry.time == time && entry.teacher == teacher)) {
      errors("teacher") = Some(s"Преподаватель $teacher занят в это время.")
    } else {
      errors("teacher") = None
    }

    if (schedule.exists(entry => entry.day == day && entry.time == time && entry.group == group)) {
      errors("group") = Some(s"Группа $group занята в это время.")
    } else {
      errors("group") = None
    }

    errors.toMap
  }

  def addSchedule(day: String, time: String, teacher: String, group: String, auditorium: String): Unit = {
    val availability = isAvailableScheduleSlot(day, time, teacher, group, auditorium)
    val errors = availability.filter(_._2.isDefined).values.flatten.toList

    if (errors.nonEmpty) {
      errors.foreach(error => println(s"Ошибка: $error"))

      val occupiedAuditoriums = schedule.filter(entry => entry.day == day && entry.time == time).map(_.auditorium)
      val occupiedTeachers = schedule.filter(entry => entry.day == day && entry.time == time).map(_.teacher)
      val occupiedGroups = schedule.filter(entry => entry.day == day && entry.time == time).map(_.group)

      println("Свободные аудитории:")
      getFreeOptions(day, time, occupiedAuditoriums, auditoriums.map(_.number)).foreach(println)

      println("Свободные преподаватели:")
      getFreeOptions(day, time, occupiedTeachers, teachers.map(_.surname)).foreach(println)

      println("Свободные группы:")
      getFreeOptions(day, time, occupiedGroups, groups.map(_.name)).foreach(println)
    } else {
      schedule = Schedule(day, time, teacher, group, auditorium) :: schedule
      println("Занятие успешно добавлено в расписание.")
    }
  }

  def removeSchedule(day: String, time: String, teacher: String, group: String, auditorium: String): Unit = {
    schedule = schedule.filterNot(entry =>
      entry.day == day &&
        entry.time == time &&
        entry.teacher == teacher &&
        entry.group == group &&
        entry.auditorium == auditorium
    )
    println("Занятие успешно удалено из расписания.")
  }

  def viewSchedule(): Unit = {
    if (schedule.isEmpty) {
      println("Расписание пусто.")
    } else {
      println("Расписание:")
      println("-------------------------------------------------------------------------------")
      println("| День     | Время      | Преподаватель | Группа         | Аудитория     |")
      println("-------------------------------------------------------------------------------")
      schedule.foreach(entry =>
        println(s"| ${entry.day}  | ${entry.time}  | ${entry.teacher}          | ${entry.group}        | ${entry.auditorium}     |")
      )
      println("-------------------------------------------------------------------------------")
    }
  }
}

