package example

// snippet example
@clam.default.command
def foo(param1: String = "foo", param2: Int) =
  println(param1)
  println(param2)

def main(args: Array[String]): Unit =
  clam.commandFor(this).parseOrExit(Array("42"))
//end snippet
