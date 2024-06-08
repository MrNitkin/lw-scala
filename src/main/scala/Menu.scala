import scala.io.StdIn.readLine
import UniversityManager._
import FileOperations._
import ScheduleOperations._

object Menu {
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

  def addScheduleMenu(): Unit = {
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
  }

  def removeScheduleMenu(): Unit = {
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
  }

  def mainMenu(): Unit = {
    var continueMenu = true
    while (continueMenu) {
      println("Главное меню:")
      println("1. Управление аудиториями")
      println("2. Управление группами")
      println("3. Управление преподавателями")
      println("4. Управление расписанием")
      println("5. Загрузить данные из файла")
      println("6. Сохранить данные в файл")
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
          println("Введите имя файла для загрузки данных:")
          val filenameLoad = readLine().trim
          loadFromFile(filenameLoad)
        case "6" =>
          println("Введите имя файла для сохранения данных:")
          val filenameSave = readLine().trim
          saveToFile(filenameSave)
        case "7" =>
          continueMenu = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  def auditoriumMenu(): Unit = {
    var continueMenu = true
    while (continueMenu) {
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
          continueMenu = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  def groupMenu(): Unit = {
    var continueMenu = true
    while (continueMenu) {
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
          continueMenu = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }

  def teacherMenu(): Unit = {
    var continueMenu = true
    while (continueMenu) {
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
          continueMenu = false
        case _ =>
          println("Неверный выбор, попробуйте снова.")
      }
    }
  }
}

