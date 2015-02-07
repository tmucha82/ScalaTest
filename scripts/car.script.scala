import com.sdc.scala.test.Car

val cars = new Car("red", true) :: new Car("blue", false) :: Nil

for (car <- cars
     if car.valid
     if car.color == "red") {
  println(car)
}