object Main {

  def main(args: Array[String]): Unit = {
    println(first_a(", "))
    println(first_b("S"))
    println(first_c(", "))

    println(second_a(weekdays, ", "))
    println(second_b(weekdays, ", "))
    println(third_a(weekdays, ", "))
    println(fourth_a(weekdays, ", "))
    println(fourth_b(weekdays, ", "))
    println(fourth_c(weekdays, ", "))

    val products: Map[String, Double] = Map("1" -> 10, "2" -> 20, "3" -> 30)
    val priceCutProducts = products.map(i => (i._1, i._2 * 0.9))
    println(products + ", " + priceCutProducts)
    println(sixth(List(1, 2, 3)))
    println(seventh(List(-7, -19, -4, 3, -2, 6, 8, 11, 9)))
    eighth((1, 1.23, "1.23b"), ", ")
    println(ninth(List(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0)))

    val optional: Map[String, Option[String]] = Map("P1" -> None, "P2" -> Some[String]("M"), "P3" -> Some[String]("F"), "P4" -> Some[String](""))
    println(optional.map(i => (i._1, if (i._2.equals(Some[String]("M"))) "Male")))
    println(optional.map(i => (i._1, if (i._2.equals(None)) "Unknown")))
    println(optional.map(i => (i._1, if (i._2.isDefined) "Known")))
    println(optional.map(i => (i._1, if (i._2.nonEmpty) "Known")))
  }

  def first_a(delim: String): String = {
    var weekday_string = weekdays.head
    for (i <- 1 to weekdays.length - 1) weekday_string += (delim + weekdays(i))

    weekday_string
  }

  def first_b(prefix: String): String = {
    val filtered_weekdays = weekdays.filter(_.startsWith(prefix))

    var weekday_string = filtered_weekdays.head
    for (i <- 1 to filtered_weekdays.length - 1) weekday_string += (", " + filtered_weekdays(i))

    weekday_string
  }

  def first_c(delim: String): String = {
    var weekday_string = weekdays.head

    var i = 1
    while (i < weekdays.length) {
      weekday_string += (delim + weekdays(i))
      i += 1
    }

    weekday_string
  }

  def first_best(): String = {
    weekdays.mkString(", ")
  }

  def second_a(list: List[String], delim: String): String = {
    if (list.length <= 1) {
      return list.last
    }

    second_a(list.dropRight(1), delim) + delim + list.last
  }

  def second_b(list: List[String], delim: String): String = {
    if (list.length <= 1) {
      return list.head
    }

    second_b(list.drop(1), delim) + delim + list.head
  }

  def third_a(list: List[String], delim: String, acc: String = ""): String = {
    if (list.length <= 1) {
      return list.last + acc
    }

    third_a(list.dropRight(1), delim, delim + list.last + acc)
  }

  def fourth_a(list: List[String], delim: String): String = {
    list.drop(1).foldLeft(list(0))(_ + delim + _)
  }

  def fourth_b(list: List[String], delim: String): String = {
    list.dropRight(1).foldRight(list.last)(_ + delim + _)
  }

  def fourth_c(list: List[String], delim: String): String = {
    list.foldLeft("")((a, b) => {
      if (b.startsWith("S"))
        if (a.equals(""))
          b
        else
          a + delim + b
      else
        a
    })
  }

  def sixth(list: List[Int]): List[Int] = {
    list.map(_ + 1)
  }

  def seventh(list: List[Double]): List[Double] = {
    list.filter(n => n >= -5 && n <= 12).map(n => if (n > 0) n else n * -1)
  }

  def eighth(tuple: (Int, Double, String), delim: String) = {
    println(tuple._1 + delim + tuple._2 + delim + tuple._3)
  }

  def ninth(list: List[Int]): List[Int] = {
    if (list.length <= 1) if (list.head == 0) return List() else return List(list.head)
    if (list.head == 0) ninth(list.tail) else List(list.head) ++ ninth(list.tail)
  }

  val weekdays = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

}