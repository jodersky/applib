
/** Hello world
 *
 * @param x foo
 * @param y bar
*/
@clam.default.command
def foo(x: Int = 2, config: Iterable[(Int, Int)] = Nil, kind: String) =
  println(x)
  println(config)

def main(args: Array[String]): Unit = clam.commandFor(this).parseOrExit(args)
