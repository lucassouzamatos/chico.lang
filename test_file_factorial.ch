var a := 12

apply + a 1 done

var c := "test 1"

fun last(N A) ->  
  apply * N A done
done

fun factorial(N) ->
  match N with
    (1) -> 1 done
    (_) -> 
      var X := apply - N 1 done
      var Y := apply factorial X done

      apply * N Y done
  done
done

apply factorial 4 done
