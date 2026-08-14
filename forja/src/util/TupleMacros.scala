package forja.util

import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.quotes

object TupleMacros:
  extension [Tpl <: Tuple](tpl: Type[Tpl])(using Quotes)
    def toList: List[Type[?]] =
      import quotes.reflect.*
      given Type[Tpl] = tpl

      val tplRepr = TypeRepr.of[Tpl]
      if tplRepr.isTupleN
      then
        tplRepr.dealias.typeArgs.map(_.asType)
      else
        tpl match
          case '[EmptyTuple] => Nil
          case '[hd *: tl] => Type.of[hd] :: Type.of[tl].toList
          case _ =>
            report.errorAndAbort(s"insufficiently precise Tuple type: ${TypeTree.of[Tpl].show}")
        end match
      end if
    end toList
  end extension

  extension (list: List[Type[?]])(using Quotes)
    def toTupleType: Type[? <: Tuple] =
      import quotes.reflect.*
      val size = list.size
      if (2 to 22).contains(size)
      then
        defn.TupleClass(size)
          .typeRef
          .appliedTo:
            list.map:
              case '[elem] =>
                TypeRepr.of[elem]
          .asType
          match
            case '[type tpl <: Tuple; tpl] =>
              Type.of[tpl]
          end match
      else
        list.foldRight[Type[? <: Tuple]](Type.of[EmptyTuple]): (hd, tl) =>
          (tl, hd).runtimeChecked match
            case ('[type tl <: Tuple; tl], '[hd]) =>
              Type.of[hd *: tl]
          end match
      end if
    end toTupleType
  end extension
end TupleMacros
