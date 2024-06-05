import java.io.{BufferedWriter, File, FileWriter}
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.matching.Regex
import java.io.IOException

// Класс для представления аудитории
case class Auditorium(number: String, capacity: Int)

// Класс для представления группы
case class Group(name: String, studentCount: Int)

// Класс для представления преподавателя
case class Teacher(surname: String)

// Класс для представления расписания
case class Schedule(day: String, time: String, teacher: String, group: String, auditorium: String)

object UniversityManager {
  var auditoriums: List[Auditorium] = List()
  var groups: List[Group] = List()
  var teachers: List[Teacher] = List()
  var schedule: List[Schedule] = List()

  // Добавление аудитории с проверкой
  def addAuditorium(number: String, capacity: Int): Unit = {
    if (isValidAuditoriumNumber(number)) {
      auditoriums = Auditorium(number, capacity) :: auditoriums
      println(s"Аудитория $number успешно добавлена.")
    } else {
      println("Некорректный номер аудитории. Номер должен состоять только из цифр и иметь от 1 до 4 цифр.")
    } 
  }

  // Удаление аудитории
  def removeAuditorium(number: String): Unit = {
    auditoriums = auditoriums.filterNot(_.number == number)
  }

  // Просмотр всех аудиторий
  def viewAuditoriums(): Unit = {
    if (auditoriums.isEmpty) {
      println("Список аудиторий пуст.")
    } else {
      println("Список аудиторий:")
      auditoriums.foreach(auditorium => println(s"Номер: ${auditorium.number}, Вместимость: ${auditorium.capacity}"))
    }
  }

  // Добавление группы с проверкой
  def addGroup(name: String, studentCount: Int): Unit = {
   if (isValidGroupName(name)) {
      groups = Group(name, studentCount) :: groups
      println(s"Группа $name успешно добавлена.")
    } else {
      println("Некорректное название группы. Название должно быть в формате 00-XX, где XX - буквенный код группы.")
    }
  }

  // Удаление группы
  def removeGroup(name: String): Unit = {
    groups = groups.filterNot(_.name == name)
  }

  // Просмотр всех групп
  def viewGroups(): Unit = {
    if (groups.isEmpty) {
      println("Список групп пуст.")
    } else {
      println("Список групп:")
      groups.foreach(group => println(s"Название: ${group.name}, Количество студентов: ${group.studentCount}"))
    }
  }

  // Добавление преподавателя с проверкой
  def addTeacher(surname: String): Unit = {
    if (isValidTeacherSurname(surname)) {
      teachers = Teacher(surname) :: teachers
      println(s"Преподаватель $surname успешно добавлен.")
    } else {
      println("Некорректная фамилия преподавателя. Фамилия должна содержать только буквы.")
    }
  }

  // Удаление преподавателя
  def removeTeacher(surname: String): Unit = {
    teachers = teachers.filterNot(_.surname == surname)
  }

  // Просмотр всех преподавателей
  def viewTeachers(): Unit = {
    if (teachers.isEmpty) {
      println("Список преподавателей пуст.")
    } else {
      println("Список преподавателей:")
      teachers.foreach(teacher => println(s"Фамилия: ${teacher.surname}"))
    }
  }

  // Добавление расписания
  def addSchedule(day: String, time: String, teacher: String, group: String, auditorium: String): Unit = {
    if (isValidScheduleEntry(day, time, teacher, group, auditorium) && isAvailableScheduleSlot(day, time, auditorium)) {
      schedule = Schedule(day, time, teacher, group, auditorium) :: schedule
      println("Занятие успешно добавлено в расписание.")
    } else {
      println("Ошибка: некорректные данные для добавления занятия.")
    }
  }

  // Удаление расписания
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

  // Просмотр расписания
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

  // Валидация фамилии преподавателя
  def isValidTeacherSurname(surname: String): Boolean = {
    surname.matches("[a-zA-Z]+")
  }

  // Валидация номера аудитории
  def isValidAuditoriumNumber(number: String): Boolean = {
    number.matches("\\d{1,4}")
  }

  // Валидация формата названия группы
  def isValidGroupName(name: String): Boolean = {
    name.matches("\\d{2}-[a-zA-Z]+")
  }

  // Проверка корректности данных расписания
  def isValidScheduleEntry(day: String, time: String, teacher: String, group: String, auditorium: String): Boolean = {
    isValidTeacherSurname(teacher) && isValidGroupName(group) && isValidAuditoriumNumber(auditorium)
  }

  // Проверка доступности слота расписания
  def isAvailableScheduleSlot(day: String, time: String, auditorium: String): Boolean = {
    !schedule.exists(entry => entry.day == day && entry.time == time && entry.auditorium == auditorium)
  }

  // Запись данных в файл
  def saveToFile(filename: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    try {
      if (!file.exists()) {
        file.createNewFile()
      }

      // Запись данных об аудиториях
      bw.write("AUDITORIUM:")
      bw.newLine()
      for (auditorium <- auditoriums) {
        bw.write(s"${auditorium.number},${auditorium.capacity}")
        bw.newLine()
      }

      // Запись данных о группах
      bw.write("GROUP:")
      bw.newLine()
      for (group <- groups) {
        bw.write(s"${group.name},${group.studentCount}")
        bw.newLine()
      }

      // Запись данных о преподавателях
      bw.write("TEACHER:")
      bw.newLine()
      for (teacher <- teachers) {
        bw.write(s"${teacher.surname}")
        bw.newLine()
      }

      // Запись данных о расписании
      bw.write("SCHEDULE:")
      bw.newLine()
      for (item <- schedule) {
        bw.write(s"${item.day},${item.time},${item.teacher},${item.group},${item.auditorium}")
        bw.newLine()
      }

      println("Данные успешно сохранены в файл.")
    } catch {
      case e: IOException =>
        println("Ошибка при записи данных в файл.")
    } finally {
      bw.close()
    }
  }

  // Чтение данных из файла
  def loadFromFile(filename: String): Unit = {
    val file = new File(filename)

    if (!file.exists()) {
      println("Файл не существует.")
      return
    }

    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().toList
      source.close()

      var auditoriumLines = List[String]()
      var groupLines = List[String]()
      var teacherLines = List[String]()
      var scheduleLines = List[String]()

      var section = "" // Переменная для отслеживания текущей секции файла (AUDITORIUM, GROUP, TEACHER, SCHEDULE)

      for (line <- lines) {
        if (line.startsWith("AUDITORIUM")) {
          section = "AUDITORIUM"
        } else if (line.startsWith("GROUP")) {
          section = "GROUP"
        } else if (line.startsWith("TEACHER")) {
          section = "TEACHER"
        } else if (line.startsWith("SCHEDULE")) {
          section = "SCHEDULE"
        } else {
          section match {
            case "AUDITORIUM" => auditoriumLines :+= line
            case "GROUP" => groupLines :+= line
            case "TEACHER" => teacherLines :+= line
            case "SCHEDULE" => scheduleLines :+= line
            case _ => // Handle error or ignore line
          }
        }
      }

    // Обработка данных аудиторий
      auditoriums = auditoriumLines.filter(_.nonEmpty).map { line =>
        val Array(number, capacity) = line.split(",")
        Auditorium(number.trim, capacity.trim.toInt)
      }

      // Обработка данных групп
      groups = groupLines.filter(_.nonEmpty).map { line =>
        val Array(name, studentCount) = line.split(",")
        Group(name.trim, studentCount.trim.toInt)
      }

      // Обработка данных преподавателей
      teachers = teacherLines.filter(_.nonEmpty).map { line =>
        Teacher(line.trim)
      }

      // Обработка данных расписания
      schedule = scheduleLines.filter(_.nonEmpty).map { line =>
        val Array(day, time, teacher, group, auditorium) = line.split(",")
        Schedule(day.trim, time.trim, teacher.trim, group.trim, auditorium.trim)
      }

      println("Данные успешно загружены из файла.")
    } catch {
      case e: IOException =>
        println("Ошибка при чтении данных из файла.")
    }
  }

  // Меню управления расписанием
  def scheduleMenu(): Unit = {
    var continue = true
    while (continue) {
      println("Меню управления расписанием:")
      println("1. Добавить расписание")
      println("2. Удалить расписание")
      println("3. Просмотреть расписание")
      println("4. Назад")

      val choice = readLine().trim

      choice match {
        case "1" =>
          println("Выберите день (M, T, W, Th, F):")
          val day = readLine().trim.toUpperCase
          println("Выберите время (1 - 7:30, 2 - 9:15, 3 - 11:00, 4 - 12:40, 5 - 14:15):")
          val timeInput = readLine().trim
          val time = timeInput match {
            case "1" => "7:30"
            case "2" => "9:15"
            case "3" => "11:00"
            case "4" => "12:40"
            case "5" => "14:15"
            case _ => ""
          }
          if (time.isEmpty) {
            println("Неверный выбор времени.")
          } else {
            println("Выберите преподавателя:")
            viewTeachers()
            val teacher = readLine().trim
            println("Выберите группу:")
            viewGroups()
            val group = readLine().trim
            println("Выберите аудиторию:")
            viewAuditoriums()
            val auditorium = readLine().trim
            addSchedule(day, time, teacher, group, auditorium)
          }
        case "2" =>
          println("Выберите день (M, T, W, Th, F):")
          val day = readLine().trim.toUpperCase
          println("Выберите время (1 - 7:30, 2 - 9:15, 3 - 11:00, 4 - 12:40, 5 - 14:15):")
          val timeInput = readLine().trim
          val time = timeInput match {
            case "1" => "7:30"
            case "2" => "9:15"
            case "3" => "11:00"
            case "4" => "12:40"
            case "5" => "14:15"
            case _ => ""
          }
          if (time.isEmpty) {
            println("Неверный выбор времени.")
          } else {
            println("Выберите преподавателя:")
            viewTeachers()
            val teacher = readLine().trim
            println("Выберите группу:")
            viewGroups()
            val group = readLine().trim
            println("Выберите аудиторию:")
            viewAuditoriums()
            val auditorium = readLine().trim
            removeSchedule(day, time, teacher, group, auditorium)
          }
        case "3" =>
          viewSchedule()
        case "4" =>
          continue = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  // Главное меню
  def mainMenu(): Unit = {
    var continue = true
    while (continue) {
      println("Главное меню:")
      println("1. Управление аудиториями")
      println("2. Управление группами")
      println("3. Управление преподавателями")
      println("4. Управление расписанием")
      println("5. Сохранить данные в файл")
      println("6. Загрузить данные из файла")
      println("7. Выйти")

      val choice = readLine().trim

      choice match {
        case "1" =>
          auditoriumMenu()
        case "2" =>
          groupMenu()
        case "3" =>
          teacherMenu()
        case "4" =>
          scheduleMenu()
        case "5" =>
          println("Введите имя файла для сохранения данных:")
          val filename = readLine().trim
          saveToFile(filename)
        case "6" =>
          println("Введите имя файла для загрузки данных:")
          val filename = readLine().trim
          loadFromFile(filename)
        case "7" =>
          continue = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  // Меню управления аудиториями
  def auditoriumMenu(): Unit = {
    var continue = true
    while (continue) {
      println("Меню управления аудиториями:")
      println("1. Добавить аудиторию")
      println("2. Удалить аудиторию")
      println("3. Просмотреть аудитории")
      println("4. Назад")

      val choice = readLine().trim

      choice match {
        case "1" =>
          println("Введите номер аудитории:")
          val number = readLine().trim
          println("Введите вместимость аудитории:")
          val capacity = readLine().trim.toInt
          addAuditorium(number, capacity)
        case "2" =>
          println("Введите номер аудитории для удаления:")
          val number = readLine().trim
          removeAuditorium(number)
        case "3" =>
          viewAuditoriums()
        case "4" =>
          continue = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  // Меню управления группами
  def groupMenu(): Unit = {
    var continue = true
    while (continue) {
      println("Меню управления группами:")
      println("1. Добавить группу")
      println("2. Удалить группу")
      println("3. Просмотреть группы")
      println("4. Назад")

      val choice = readLine().trim

      choice match {
        case "1" =>
          println("Введите название группы:")
          val name = readLine().trim
          println("Введите количество студентов в группе:")
          val studentCount = readLine().trim.toInt
          addGroup(name, studentCount)
        case "2" =>
          println("Введите название группы для удаления:")
          val name = readLine().trim
          removeGroup(name)
        case "3" =>
          viewGroups()
        case "4" =>
          continue = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  // Меню управления преподавателями
  def teacherMenu(): Unit = {
    var continue = true
    while (continue) {
      println("Меню управления преподавателями:")
      println("1. Добавить преподавателя")
      println("2. Удалить преподавателя")
      println("3. Просмотреть преподавателей")
      println("4. Назад")

      val choice = readLine().trim

      choice match {
        case "1" =>
          println("Введите фамилию преподавателя:")
          val surname = readLine().trim
          addTeacher(surname)
        case "2" =>
          println("Введите фамилию преподавателя для удаления:")
          val surname = readLine().trim
          removeTeacher(surname)
        case "3" =>
          viewTeachers()
        case "4" =>
          continue = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    mainMenu()
  }
}
