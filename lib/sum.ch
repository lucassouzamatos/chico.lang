@module math

@exports
# @description
#   The sum function should be called as:
# 
#   @import math
#
#   var A := 1
#   var B := 2
#
#   apply 
#     > math:sum A B
#    |> pipeline operator receive the last result expression
#   done
#
fun sum(A B) -> apply + A B done
