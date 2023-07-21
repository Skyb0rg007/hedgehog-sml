
structure Real64 = Real

structure Either =
struct
  datatype ('a, 'b) either = INL of 'a | INR of 'b
end
