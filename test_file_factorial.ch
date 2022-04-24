fun factorial(C N) ->
  match N with
    (1) -> 1 done
    (_) -> 
      var X := apply - N 1 done
      var Y := apply C C X done

      apply * N Y done
  done
done

apply factorial factorial 4 done
