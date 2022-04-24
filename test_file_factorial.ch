fun recur (F) -> 
  fun Recursion (X) ->
    apply X X done
  done

  fun Bind (X) ->
    fun Inner (Y) ->
      var A := apply X X done
      apply A Y done
    done

    apply F Inner done
  done

  apply Recursion Bind done
done

fun builder(Self) ->
  fun Apply (N) ->
    match N with
      (1) -> 1 done
      (_) -> 
        var X := apply - N 1 done
        var Y := apply Self X done

        apply * N Y done
    done
  done
done

var factorial := apply recur builder done

apply factorial 4 done
