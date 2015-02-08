import com.sdc.scala.car.Car

val cars = new Car("red", true) :: new Car("blue", false) :: Nil

for (car <- cars
     if car.valid
     if car.color == "red") {
  println(car)
}