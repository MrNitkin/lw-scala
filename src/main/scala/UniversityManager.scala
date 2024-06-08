import Menu.mainMenu
import Utils._

object UniversityManager {
  var auditoriums: List[Auditorium] = List()
  var groups: List[Group] = List()
  var teachers: List[Teacher] = List()

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

  def main(args: Array[String]): Unit = {
    mainMenu()
  }
}