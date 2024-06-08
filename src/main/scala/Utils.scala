object Utils {
  def isValidTeacherSurname(surname: String): Boolean = {
    surname.matches("[a-zA-Z]+")
  }

  def isValidAuditoriumNumber(number: String): Boolean = {
    number.matches("\\d{1,4}")
  }

  def isValidGroupName(name: String): Boolean = {
    name.matches("\\d{2}-[a-zA-Z]+")
  }

  def isValidScheduleEntry(day: String, time: String, teacher: String, group: String, auditorium: String): Boolean = {
    isValidTeacherSurname(teacher) && isValidGroupName(group) && isValidAuditoriumNumber(auditorium)
  }
}