import Menu.mainMenu
import Utils._

object UniversityManager {
  var auditoriums: List[Auditorium] = List()
  var groups: List[Group] = List()
  var teachers: List[Teacher] = List()
  var schedule: List[Schedule] = List()

  def addAuditorium(number: String, capacity: Int): Unit = {
    if (isValidAuditoriumNumber(number)) {
      auditoriums = Auditorium(number, capacity) :: auditoriums
      println(s"Аудитория $number успешно добавлена.")
    } else {
      println("Некорректный номер аудитории. Номер должен состоять только из цифр и иметь от 1 до 4 цифр.")
    } 
  }

  def removeAuditorium(number: String): Unit = {
    auditoriums = auditoriums.filterNot(_.number == number)
  }

  def viewAuditoriums(): Unit = {
    if (auditoriums.isEmpty) {
      println("Список аудиторий пуст.")
    } else {
      println("Список аудиторий:")
      auditoriums.foreach(auditorium => println(s"Номер: ${auditorium.number}, Вместимость: ${auditorium.capacity}"))
    }
  }

  def addGroup(name: String, studentCount: Int): Unit = {
    if (isValidGroupName(name)) {
      groups = Group(name, studentCount) :: groups
      println(s"Группа $name успешно добавлена.")
    } else {
      println("Некорректное название группы. Название должно быть в формате 00-XX, где XX - буквенный код группы.")
    }
  }

  def removeGroup(name: String): Unit = {
    groups = groups.filterNot(_.name == name)
  }

  def viewGroups(): Unit = {
    if (groups.isEmpty) {
      println("Список групп пуст.")
    } else {
      println("Список групп:")
      groups.foreach(group => println(s"Название: ${group.name}, Количество студентов: ${group.studentCount}"))
    }
  }

  def addTeacher(surname: String): Unit = {
    if (isValidTeacherSurname(surname)) {
      teachers = Teacher(surname) :: teachers
      println(s"Преподаватель $surname успешно добавлен.")
    } else {
      println("Некорректная фамилия преподавателя. Фамилия должна содержать только буквы.")
    }
  }

  def removeTeacher(surname: String): Unit = {
    teachers = teachers.filterNot(_.surname == surname)
  }

  def viewTeachers(): Unit = {
    if (teachers.isEmpty) {
      println("Список преподавателей пуст.")
    } else {
      println("Список преподавателей:")
      teachers.foreach(teacher => println(s"Фамилия: ${teacher.surname}"))
    }
  }

  def addSchedule(day: String, time: String, teacher: String, group: String, auditorium: String): Unit = {
    if (isValidScheduleEntry(day, time, teacher, group, auditorium) && isAvailableScheduleSlot(day, time, auditorium)) {
      schedule = Schedule(day, time, teacher, group, auditorium) :: schedule
      println("Занятие успешно добавлено в расписание.")
    } else {
      println("Ошибка: некорректные данные для добавления занятия.")
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

  def main(args: Array[String]): Unit = {
    mainMenu()
  }
}