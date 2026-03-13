def fact (n: i32): i32 = reduce (*) 1 (1...n)
def main (n: i32): i32 = fact n
