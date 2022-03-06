apply + 3 5
apply + 5 9
apply * 9 0
apply / 10 2

# functions definition
fun sum (a b) -> apply + a b
var result := apply sum 1 2

# variable declaration
var a := 1
var b := 2

# type annotations
factorial :: (int int) -> int
             (int int) -> void

fun factorial (number index) where number == 0 -> 1
fun factorial (number index) where number < 0 -> void
fun factorial (number index) ->
    var x := apply - number 1
    var y := apply factorial x

    apply * number y
