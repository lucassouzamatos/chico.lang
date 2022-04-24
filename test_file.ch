let a = 12

apply + a 1 done

let c = "test 1"

fun last(N A) ->
  apply * N A done
done

fun factorial(N) ->
  let X = apply - N 1 done
  let Y = apply last X N done

  Y done
done

apply factorial 4 done
