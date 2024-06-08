import java.io.{BufferedWriter, File, FileWriter, IOException}
import scala.io.Source
import UniversityManager._
import ScheduleOperations._

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

  def loadFromFile(filename: String): Unit = {
    val file = new File(filename)
    if (!file.exists()) {
      println("Файл не найден.")
      return
    }

    try {
      val source = Source.fromFile(file)
      val lines = source.getLines()

      var currentSection = ""

      for (line <- lines) {
        line match {
          case "AUDITORIUM:" => currentSection = "AUDITORIUM"
          case "GROUP:" => currentSection = "GROUP"
          case "TEACHER:" => currentSection = "TEACHER"
          case "SCHEDULE:" => currentSection = "SCHEDULE"
          case _ =>
            currentSection match {
              case "AUDITORIUM" =>
                val parts = line.split(",")
                if (parts.length == 2) {
                  val number = parts(0)
                  val capacity = parts(1).toInt
                  auditoriums = Auditorium(number, capacity) :: auditoriums
                }
              case "GROUP" =>
                val parts = line.split(",")
                if (parts.length == 2) {
                  val name = parts(0)
                  val studentCount = parts(1).toInt
                  groups = Group(name, studentCount) :: groups
                }
              case "TEACHER" =>
                teachers = Teacher(line) :: teachers
              case "SCHEDULE" =>
                val parts = line.split(",")
                if (parts.length == 5) {
                  val day = parts(0)
                  val time = parts(1)
                  val teacher = parts(2)
                  val group = parts(3)
                  val auditorium = parts(4)
                  schedule = Schedule(day, time, teacher, group, auditorium) :: schedule
                }
              case _ =>
            }
        }
      }

      source.close()
      println("Данные успешно загружены из файла.")
    } catch {
      case e: IOException =>
        println("Ошибка при чтении данных из файла.")
    }
  }
}
