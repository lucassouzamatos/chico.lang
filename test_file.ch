var a := 12

apply + a 1 done

var c := "test 1"

fun last(N A) ->
  apply * N A done
done

fun factorial(N) ->
  var X := apply - N 1 done
  var Y := apply last X N done

  Y done
done

apply factorial 4 done
