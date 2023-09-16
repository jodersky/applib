package confuse.macros

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import confuse.Value
import confuse.Config
import confuse.FieldError
import confuse.Result
import confuse.api.ReaderApi

private def getDefaultParams(using qctx: Quotes)
  (method: qctx.reflect.Symbol): Map[qctx.reflect.Symbol, qctx.reflect.Term] =
  import qctx.reflect.*
  val pairs = for
    (param, idx) <- method.paramSymss.flatten.zipWithIndex
    if (param.flags.is(Flags.HasDefault))
  yield
    val term = if method.isClassConstructor then
      val defaultName = s"$$lessinit$$greater$$default$$${idx + 1}"
      Ref(method.owner.companionModule.methodMember(defaultName).head)
    else
      val defaultName = s"${method.name}$$default$$${idx + 1}"
      Ref(method.owner.methodMember(defaultName).head)
    param -> term
  pairs.toMap

def derivedImpl[Api <: ReaderApi : Type, A: Type](using qctx: Quotes)(api: Expr[Api]): Expr[ReaderApi#Reader[A]] =
  import qctx.reflect.*

  val tpe = TypeRepr.of[A]
  if !(tpe <:< TypeRepr.of[Product]) then
    report.error(s"${tpe.show} is not a product type")
    return '{???}

  val constructor = tpe.typeSymbol.companionModule.methodMember("apply").head

  val fieldReaders: List[List[Expr[ReaderApi#Reader[_]]]] =
    for params <- constructor.paramSymss yield
      for param <- params yield
        val readerType = TypeSelect(api.asTerm, "Reader").tpe.appliedTo(param.termRef.widenTermRefByName)
        Implicits.search(readerType) match
          case iss: ImplicitSearchSuccess => iss.tree.asExprOf[ReaderApi#Reader[_]]
          case _ =>
            report.error(s"No ${readerType.show} available for parameter ${param.name}.", param.pos.get)
            '{???}
      end for
    end for

  val defaults = getDefaultParams(constructor)



  // // val errors =
  // // var arg1: Int = 0
  // // summon[Reader[Int]].read(c("name1")) match
  // //   case ok => arg1 = ok
  // //   case error(msg) => errors += msg
  // //
  // val sym = Symbol.newVal(Symbol.spliceOwner, "v", TypeRepr.of[Int], Flags.Mutable, Symbol.spliceOwner)
  // ValDef(sym, Some('{0}.asTerm))
  // // Assign(Ref(sym), '{o}.asTerm)


  // val syms = Symbol.newVal(Symbol.spliceOwner, "v", TypeRepr.of[Int], Flags.Mutable, Symbol.spliceOwner)

  // '{
  //   val errors = collection.mutable.ArrayBuffer.empty[FieldError]
  //   ${

  //     ValDef(sym, Some('{0}.asTerm))
  //   }
  //   ${
  //     ValDef.let(Symbol.spliceOwner, Nil){ xs =>


  //       call(xs)
  //       ???
  //     }
  //     ???
  //   }
  // }

  // ValDef.let(Symbol.spliceOwner, Nil)(xs =>
  //   ???
  // )



  // for param <- constructor.
  // for param <- constructor.paramSymss.flatten yield
  //   // val v = Symbol.newVal(Symbol.spliceOwner, reflect.)


  // name: String
  // reader: Reader[?]
  // default: Option[() => ?]

  // val c1 = config.getValue(name)
  // var arg1: Int = 0
  // var arg2: = null
  // val arg1 = reader.read(c1)
  //

  // reader.read(name) match
  //   case Success(a) =>
  //



  '{
    (value: Value, path: List[String]) => value match
      case c: Config =>
        val p = $api
        val names: Seq[String] = ${Expr(constructor.paramSymss.flatten.map(_.name))}
        val readers: Seq[p.Reader[_]] = ${Expr.ofSeq(fieldReaders.flatten)}.asInstanceOf[Seq[p.Reader[_]]]

        val errors = collection.mutable.ArrayBuffer.empty[FieldError]
        val ccArgs = new Array[Any](${Expr(constructor.paramSymss.map(_.size).sum)})



        for i <- 0 until ccArgs.size do
          readers(i).read(c.getValue(names(i)), path :+ names(i)) match
            case Result.Success(value) => ccArgs(i) = value
            case Result.Error(errs) => errors ++= errs

        if errors.isEmpty then
          try
            val s = ${
              val base: Term = Ref(constructor)
              var i = 0
              val accesses: List[List[Term]] =
                for params <- constructor.paramSymss yield
                  for param <- params yield

                    param.termRef.widenTermRefByName.asType match
                      case '[t] =>
                        val expr = '{
                          ccArgs(${Expr(i)}).asInstanceOf[t]
                        }
                        i += 1
                        expr.asTerm
              val application = accesses.foldLeft(base)((lhs, args) => Apply(lhs, args))
              application.asExprOf[A]
            }
            Result.Success(s)
          catch
            case ex: IllegalArgumentException =>
              val err = FieldError.Validation(path, value, ex.getMessage())
              Result.Error(Seq(err))
        else Result.Error(errors.toSeq)

      case _ => Result.single(FieldError.Mismatch(path, value, "a config map"))
  }
