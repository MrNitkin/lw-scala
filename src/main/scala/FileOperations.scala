import java.io.{BufferedWriter, File, FileWriter}
import java.io.IOException
import java.io.File
import scala.io.Source
import java.io.IOException

object FileOperations {
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
      for (auditorium <- UniversityManager.auditoriums) {
        bw.write(s"${auditorium.number},${auditorium.capacity}")
        bw.newLine()
      }

      // Запись данных о группах
      bw.write("GROUP:")
      bw.newLine()
      for (group <- UniversityManager.groups) {
        bw.write(s"${group.name},${group.studentCount}")
        bw.newLine()
      }

      // Запись данных о преподавателях
      bw.write("TEACHER:")
      bw.newLine()
      for (teacher <- UniversityManager.teachers) {
        bw.write(s"${teacher.surname}")
        bw.newLine()
      }

      // Запись данных о расписании
      bw.write("SCHEDULE:")
      bw.newLine()
      for (item <- UniversityManager.schedule) {
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
      UniversityManager.auditoriums = auditoriumLines.filter(_.nonEmpty).map { line =>
        val Array(number, capacity) = line.split(",")
        Auditorium(number.trim, capacity.trim.toInt)
      }

      // Обработка данных групп
      UniversityManager.groups = groupLines.filter(_.nonEmpty).map { line =>
        val Array(name, studentCount) = line.split(",")
        Group(name.trim, studentCount.trim.toInt)
      }

      // Обработка данных преподавателей
      UniversityManager.teachers = teacherLines.filter(_.nonEmpty).map { line =>
        Teacher(line.trim)
      }

      // Обработка данных расписания
      UniversityManager.schedule = scheduleLines.filter(_.nonEmpty).map { line =>
        val Array(day, time, teacher, group, auditorium) = line.split(",")
        Schedule(day.trim, time.trim, teacher.trim, group.trim, auditorium.trim)
      }

      println("Данные успешно загружены из файла.")
    } catch {
      case e: IOException =>
        println("Ошибка при чтении данных из файла.")
    }
  }
}

