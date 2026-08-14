package forja

import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.annotation.publicInBinary
import scala.quoted.quotes
import forja.util.TupleMacros.*
import forja.Lang2.SumMeta
import forja.Lang2.ErasedNode
import forja.util.ExprMacros.summonOrAbort
import scala.language.experimental.erasedDefinitions
import scala.annotation.tailrec
import scala.collection.mutable

@publicInBinary
private[forja] object MetaMacros:
  def findCompanionType[T: Type](using Quotes)(): Type[?] =
    import quotes.reflect.*
    val n2Repr = TypeRepr.of[T]
    def err: Nothing = 
      report.errorAndAbort(s"Could not find companion module for ${n2Repr.show}")
    val companion = n2Repr match
      case TypeRef(base, name) =>
        val bSym = base.sym
        val mem = bSym.fieldMember(name) match
          case s if s.isNoSymbol =>
            bSym.methodMember(name) match
              case List(meth) => meth
              case _ => err
            end match
          case s => s
        end mem
        if mem.isNoSymbol
        then err
        base.select(mem)
      case other =>
        err
    end companion

    // println(s"companion ${TypeRepr.of[T].show(using Printer.TypeReprStructure)} --> ${companion.show(using Printer.TypeReprStructure)}")
    companion.asType
  end findCompanionType

  def findCompanion[T : Type](using Quotes)(): Expr[Any] =
    import quotes.reflect.*
    findCompanionType[T]() match
      case '[companion] =>
        TypeRepr.of[companion] match
          case ref: TermRef =>
            Ident(ref).asExpr
        end match
    end match
  end findCompanion

  def isCompanionImpl[C : Type, T : Type](using Quotes): Expr[Lang2.IsCompanion[C, T]] =
    import quotes.reflect.*
    (TypeRepr.of[C], TypeRepr.of[T]) match
      case (TermRef(cBase, cName), TypeRef(tBase, tName)) if cBase =:= tBase && cName == tName =>
        '{ new Lang2.IsCompanion[C, T] }
      case _ =>
        report.errorAndAbort(s"${TypeRepr.of[C].show} is not the companion of ${TypeRepr.of[T].show}")
    end match
  end isCompanionImpl

  def findTermCasesRaw[T : Type](using Quotes)(): List[(String, Type[?])] =
    import quotes.reflect.*
    val tRepr = TypeRepr.of[T]
    tRepr
      .typeSymbol
      .methodMembers
      .sortBy(_.pos.map(p => (p.sourceFile.path, p.start, p.end)))
      .flatMap: meth =>
        meth.typeRef.asType match
          case '[F[?]] =>
            tRepr.select(meth).asType match
              case '[F[t]] =>
                List((meth.name, Type.of[t]))
            end match
          case _ => Nil
        end match
  end findTermCasesRaw

  extension (tpe: Type[?])(using Quotes)
    def asTypeRepr: quotes.reflect.TypeRepr =
      tpe match
        case '[tpe] =>
          quotes.reflect.TypeRepr.of[tpe]
    end asTypeRepr
  end extension

  extension (using Quotes)(repr: quotes.reflect.TypeRepr)
    def asTypeOf[T : Type]: Type[? <: T] =
      repr.asType match
        case '[type t <: T; t] =>
          Type.of[t]
      end match
    end asTypeOf
  end extension

  def buildTerm[Cls <: T : Type, T : Type](using Quotes)(termCasesErased: List[(String, Type[?])]): Expr[Term[T]] =
    '{
      new forja.Term(locally {
        new Lang2.TermMeta[T]:
          def Companion: Companion =
            ${ findCompanion[T]() }.asInstanceOf[Companion]
          end Companion

          @publicInBinary
          def applyImpl(arg: Any): T =
            ${
              import quotes.reflect.*
              // Intentional dupe: this is for cases 1 and 2.
              // Case 3 has a nested quote context
              val dataClsSym = TypeRepr.of[Cls].typeSymbol
              val termCasesErasedSize = termCasesErased.size
              termCasesErasedSize match
                case 0 =>
                  Apply(
                    Select(New(TypeIdent(dataClsSym)), dataClsSym.primaryConstructor),
                    Nil,
                  ).asExprOf[T]
                case 1 =>
                  termCasesErased.head._2 match
                    case '[tpe] =>
                      Apply(
                        Select(New(TypeIdent(dataClsSym)), dataClsSym.primaryConstructor),
                        List('{ arg.asInstanceOf[tpe] }.asTerm),
                      ).asExprOf[T]
                  end match
                case _ =>
                  val tpl =
                    if termCasesErased.sizeIs >= 2 && termCasesErased.sizeIs <= 22
                    then
                      defn.TupleClass(termCasesErased.size)
                        .typeRef
                        .appliedTo(termCasesErased.map(_._2.asTypeRepr))
                    else TypeRepr.of[Tuple].typeSymbol.typeRef
                  end tpl
                  tpl.asType match
                    case '[type tpl <: Tuple; tpl] =>
                      '{
                        val erasedCases = arg.asInstanceOf[tpl]
                        ${
                          import quotes.reflect.*
                          val dataClsSym = TypeRepr.of[Cls].typeSymbol
                          val tplSym = TypeRepr.of[tpl].typeSymbol
                          // report.errorAndAbort(tplSym.fieldMembers.map(_.name).mkString(", "))
                          Apply(
                            Select(New(TypeIdent(dataClsSym)), dataClsSym.primaryConstructor),
                            termCasesErased
                              .map(_._2)
                              .zipWithIndex
                              .map { (tpe, i) =>
                                tpe match
                                  case '[tpe] =>
                                    tplSym.fieldMember(s"_${i + 1}") match
                                      case accessor if !accessor.isNoSymbol =>
                                        '{ ${ '{ erasedCases }.asTerm.select(accessor).asExpr }.asInstanceOf[tpe] }.asTerm
                                      case _ =>
                                        '{ erasedCases.productElement(${ Expr(i) }).asInstanceOf[tpe] }.asTerm
                                    end match
                                end match
                              }
                              .toList,
                          )
                            .asExprOf[T]
                        }
                      }
                  end match
              end match
            }
          end applyImpl

          @publicInBinary
          def unapplyImpl(t: T): Any =
            ${
              import quotes.reflect.*
              val dataClsSym = TypeRepr.of[Cls].typeSymbol
              val termCasesErasedSize = termCasesErased.size
              val underlyingT = '{ t.asInstanceOf[Cls] }.asTerm
              termCasesErasedSize match
                case 0 => '{ true }
                case 1 =>
                  val fld = dataClsSym.fieldMember(termCasesErased.head._1)
                  underlyingT.select(fld).asExpr
                case _ =>
                  Expr.ofTupleFromSeq:
                    termCasesErased
                      .map(_._1)
                      .map: fldName =>
                        val fld = dataClsSym.fieldMember(fldName)
                        underlyingT.select(fld).asExpr
              end match
            }
          end unapplyImpl
        end new
      })
    }
  end buildTerm

  def termDerivedImpl[T : Type](using Quotes): Expr[Term[T]] =
    import quotes.reflect.*

    val tRepr = TypeRepr.of[T]
    tRepr.classSymbol match
      case None =>
        report.errorAndAbort(s"${tRepr.show} does not refer to a class")
      case Some(clsSym) =>
        val termCasesRaw = findTermCasesRaw[T]()
        val termCasesF = termCasesRaw.map: (name, tp) =>
          tp match
            case '[tp] => (name, TypeRepr.of[F[tp]])
          end match
        end termCasesF
        val termCasesErased = termCasesRaw.map: (name, tp) =>
          tp match
            case '[tp] =>
              Expr.summonOrAbort[TypeAdjust.Erased[tp]] match
                case '{ $_ : TypeAdjust.Erased.Aux[?, tpe] } =>
                  (Symbol.freshName(name), TypeRepr.of[tpe])
          end match
        end termCasesErased

        val dataName = Symbol.freshName("data")
        val dataParents = List(TypeTree.of[T], TypeTree.of[Lang2.ErasedNode])
        val dataClsSym = Symbol.newClass(
          owner = Symbol.spliceOwner,
          name = clsSym.name,
          parents = _ => dataParents.map(_.tpe),
          decls = { selfSym =>
            termCasesF.map { (name, tpe) =>
              Symbol.newMethod(
                parent = selfSym,
                name = name,
                tpe = ByNameType(tpe),
              )
            }
            ++ List(
              Symbol.newMethod(
                parent = selfSym,
                name = "toString",
                tpe = MethodType(MethodTypeKind.Plain)(Nil)(_ => Nil, _ => TypeRepr.of[String]),
                flags = Flags.Override,
                privateWithin = Symbol.noSymbol,
              ),
            )
          },
          selfType = None,
          clsFlags = Flags.Final,
          clsPrivateWithin = Symbol.noSymbol,
          conParams = termCasesErased,
        )
        val dataClsDecl = ClassDef(
          cls = dataClsSym,
          parents = dataParents,
          body = termCasesRaw.zip(termCasesErased).map {
            case ((name, tpe), (erasedName, erasedTpe)) =>
              val erasedSym = dataClsSym.fieldMember(erasedName)
              val methSym = dataClsSym.methodMember(name).head
              given Quotes = methSym.asQuotes
              tpe match
                case '[tpe] =>
                  DefDef(
                    methSym,
                    { paramss =>
                      assert(paramss.isEmpty)
                      Some:
                        '{ F[tpe](${ This(dataClsSym).select(erasedSym).asExpr }) }.asTerm
                    },
                  )
              end match
          } ++ List(
            DefDef(
              dataClsSym.methodMember("toString").head,
              { _ =>
                val owner = dataClsSym.methodMember("toString").head
                given Quotes = owner.asQuotes
                import quotes.reflect.*
                Some:
                  '{
                    val builder = StringBuilder()
                    builder ++= ${ Expr(clsSym.name) }
                    builder += '('
                    ${
                      Expr.ofList:
                        termCasesErased
                          .map: (name, tpe) =>
                            This(dataClsSym)
                              .select(dataClsSym.fieldMember(name))
                              .asExpr
                    }.addString(builder, ", ")
                    builder += ')'
                    builder.result()
                  }
                  .asTerm
              },
            )
          ),
        )
        Block(
          List(
            dataClsDecl,
          ),
          locally {
            dataClsSym.typeRef.asType match
              case '[type dataCls <: T; dataCls] =>
                buildTerm[dataCls, T](termCasesErased.map((name, tpe) => name -> tpe.asType))
                  .asTerm
            end match
          },
        )
        .asExprOf[forja.Term[T]]
    end match
  end termDerivedImpl

  def termMetaImpl[L <: Lang2 : Type, T : Type](term: Expr[Term[T]])(using Quotes): Expr[Lang2.TermMeta[T]] =
    import quotes.reflect.*

    val casesRaw = findTermCasesRaw[T]()
    val cases = casesRaw
      .map(_._2)
      .map:
        case '[cs] =>
          Expr.summonOrAbort[TypeAdjust.Effective[L, cs]] match
            case '{ $_ : TypeAdjust.Effective.Aux[?, ?, cs2] } =>
              TypeRepr.of[cs2]
          end match
    end cases

    val labels = casesRaw
      .map(_._1)
      .map(label => ConstantType(StringConstant(label)).asType)
      .toTupleType
    end labels

    (findCompanionType[T](), cases.map(_.asType).toTupleType, labels).runtimeChecked match
      case ('[companion], '[type cases <: Tuple; cases], '[type labels <: Tuple; labels]) =>
        '{ ${ term }.erasedMeta.asInstanceOf[Lang2.TermMeta[T] {
          type Companion = companion
          type Cases = cases
          type Labels = labels
        }] }
    end match
  end termMetaImpl

  def findSumCasesRaw[T : Type](using Quotes)(): List[(String, Type[?], Int)] =
    val tCompanion = findCompanionType[T]().asTypeRepr
    val memberSyms = tCompanion
      .termSymbol
      .typeMembers
      .filter: elemCandidate =>
        elemCandidate.typeRef.asType match
          case '[ec] =>
            Expr.summon[forja.Term[ec] | Sum[ec]] match
              case Some(_) => true
              case None => false
            end match
        end match
      .sortBy(_.name)
    end memberSyms
    memberSyms
      .map: sym =>
        (sym.name, tCompanion.select(sym).asType)
      .zipWithIndex
      .map(_ :* _)
  end findSumCasesRaw

  def sumDerivedImpl[T : Type](using Quotes): Expr[Sum[T]] =
    import quotes.reflect.*

    val tRepr = TypeRepr.of[T]
    tRepr.classSymbol match
      case None =>
        report.errorAndAbort(s"${tRepr.show} does not refer to a class")
      case Some(clsSym) =>
        findSumCasesRaw[T]() // smoke test
        val ordinalName = Symbol.freshName("ordinal")
        val elemName = Symbol.freshName("elem")
        val dataParents = List(TypeTree.of[T], TypeTree.of[Lang2.ErasedNode])
        val dataClsSym = Symbol.newClass(
          owner = Symbol.spliceOwner,
          name = clsSym.name,
          parents = _ => dataParents.map(_.tpe),
          decls = selfSym =>
            Nil,
          selfType = None,
          clsFlags = Flags.Final,
          clsPrivateWithin = Symbol.noSymbol,
          conParams = List(
            ordinalName -> TypeRepr.of[Int],
            elemName -> TypeRepr.of[Lang2.ErasedNode],
          ),
        )
        val ordinalSym = dataClsSym.declaredField(ordinalName)
        assert(!ordinalSym.isNoSymbol)
        val dataClsDecl = ClassDef(
          cls = dataClsSym,
          parents = dataParents,
          body = Nil,
        )
        Block(
          List(
            dataClsDecl,
          ),
          '{
            new forja.Sum(locally {
              new Lang2.SumMeta[T]:
                @publicInBinary
                def applyImpl(idx: Int, elem: ErasedNode): T =
                  ${
                    Apply(
                      Select(New(TypeIdent(dataClsSym)), dataClsSym.primaryConstructor),
                      List('{ idx }.asTerm, '{ elem }.asTerm),
                    )
                    .asExprOf[T]
                  }
                end applyImpl

                def Companion: Companion =
                  ${ findCompanion[T]() }.asInstanceOf[Companion]
                end Companion

                extension (T: T)
                  def ordinal: Int =
                    ${
                      TypeIdent(dataClsSym).tpe.asType match
                        case'[dataCls] =>
                          '{ T.asInstanceOf[dataCls] }
                            .asTerm
                            .select(ordinalSym)
                            .asExprOf[Int]
                    }
                  end ordinal
                end extension
              end new
            })
          }
          .asTerm,
        )
        .asExprOf[forja.Sum[T]]
    end match
  end sumDerivedImpl

  def sumMetaImpl[L <: Lang2 : Type, T : Type](sum: Expr[Sum[T]])(using Quotes): Expr[Lang2.SumMeta[T]] =
    import quotes.reflect.*

    val casesRaw = findSumCasesRaw[T]()
    val cases = casesRaw
      .map(_._2)
      .map:
        case '[cs] =>
          Expr.summonOrAbort[TypeAdjust.Effective[L, cs]] match
            case '{ $_ : TypeAdjust.Effective.Aux[?, ?, cs2] } =>
              TypeRepr.of[cs2]
          end match
    end cases

    (findCompanionType[T](), cases.map(_.asType).toTupleType).runtimeChecked match
      case ('[companion], '[type cases <: Tuple; cases]) =>
        '{ ${ sum }.erasedMeta.asInstanceOf[SumMeta[T] {
          type Companion = companion
          type Cases = cases
        }] }
    end match
  end sumMetaImpl

  def sumApplyImpl[Cases <: Tuple : Type, T : Type](using Quotes)(meta: Expr[SumMeta[T]], elem: Expr[Tuple.Union[Cases]]): Expr[T] =
    elem match
      case '{ type t <: Tuple.Union[Cases]; $elem : t } =>
        val idx = Type.of[Cases]
          .toList
          .indexWhere:
            case '[`t`] => true
            case _ => false
        end idx
        '{
          val boundMeta = ${ meta }
          ${
            if idx == -1
            then
              import quotes.reflect.*
              val caseDefs = Type.of[Cases]
                .toList
                .zipWithIndex
                .map: p =>
                  p.runtimeChecked match
                    case ('[cs], idx) =>
                      CaseDef(
                        pattern = Typed(Wildcard(), TypeTree.of[cs]),
                        guard = None,
                        rhs = Literal(IntConstant(idx)),
                      )
                  end match
              end caseDefs

              '{
                boundMeta.applyImpl(
                  ${
                    Match(
                      selector = elem.asTerm,
                      cases = caseDefs,
                    )
                    .asExprOf[Int]
                  },
                  ${ elem }.asInstanceOf[ErasedNode],
                )
              }
            else '{ boundMeta.applyImpl(${ Expr(idx) }, ${ elem }.asInstanceOf[Lang2.ErasedNode]) }
          }
        }
    end match
  end sumApplyImpl

  extension (using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
    def sym: quotes.reflect.Symbol =
      if !repr.termSymbol.isNoSymbol
      then repr.termSymbol
      else repr.typeSymbol
    end sym
  end extension

  def isNodeImpl[N : Type](using Quotes): Expr[Lang2.IsNode.Aux[N, ?, ?]] =
    import quotes.reflect.*

    val langRepr = TypeRepr.of[Lang2]
    val implRepr = TypeRepr.of[Lang2.Impl[?]]
    val extRepr = TypeRepr.of[Lang2.Ext]

    // println(s"isNode ${TypeRepr.of[N].show}")

    var path: List[String] = Nil
    val seenNodes = mutable.ArrayBuffer[TypeRepr]()
    seenNodes += TypeRepr.of[N]

    @tailrec
    def impl(repr: TypeRepr): Type[?] =
      // println(repr.show(using Printer.TypeReprStructure))
      // inline to allow tailrec
      inline def terminateAt(top: TypeRepr): Type[?] =
        if top <:< implRepr
        then top.asType
        else if top <:< extRepr
        then
          // TODO: more error handling?
          top match
            case TermRef(nextBase, _) =>
              nextBase.asType match
                case '[type nextBase <: Lang2; nextBase] =>
                  val restart = launderNodeImplRaw[nextBase](path).asTypeRepr
                  // Capture cycles, where the top most definition is an alias to something lower down
                  val lastIdx = seenNodes.indices.last
                  seenNodes.indexWhere(_ =:= restart) match
                    case -1 =>
                      // If we haven't seen this node before, take note and recurse up the type.
                      // Start the path over since we found a new leaf node.
                      seenNodes += restart
                      path = Nil
                      impl(restart)
                    case `lastIdx` =>
                      // The extension just exports our node in the same place.
                      // Keep the same path and go up another level.
                      // (will immediately check the top case and take up back to terminateAt)
                      impl(nextBase)
                    case idx =>
                      report.errorAndAbort(s"Cycle detected in the sequence of overrides ${seenNodes.iterator.map(_.show).mkString("; ")}")
                  end match
              end match
          end match
        else
          // println(s"Found ${top.show} which is neither ${implRepr.show} not ${extRepr.show}")
          report.errorAndAbort(s"Found ${top.show} which is neither ${implRepr.show} not ${extRepr.show}")
        end if 
      end terminateAt

      if repr <:< langRepr
      then
        return terminateAt(repr)
      end if

      def termFromThis(ths: ThisType): TermRef =
        // println(s"termFromThis ${ths.show(using Printer.TypeReprStructure)}")

        ths.typeSymbol.companionModule.termRef
      end termFromThis

      // Gather names, which are the same for any projection
      repr match
        case TermRef(_, name) => path = name :: path
        case TypeRef(_, name) => path = name :: path
        case _ =>
          // println(s"stop naming: ${repr.show(using Printer.TypeReprStructure)}")
          report.errorAndAbort(s"stop naming: ${repr.show(using Printer.TypeReprStructure)}")
      end match

      // Actually inspect the structure
      repr match
        case TermRef(base: TermRef, _) =>
          impl(base)
        case TermRef(top, _) if top <:< langRepr =>
          terminateAt(top)
        case TermRef(ths: ThisType, _) =>
          impl(termFromThis(ths))

        case TypeRef(base: TermRef, _) =>
          impl(base)
        case TypeRef(top, _) if top <:< langRepr =>
          terminateAt(top)
        case TypeRef(ths: ThisType, _) =>
          impl(termFromThis(ths))
        case _ =>
          // println(s"stop reduction: ${repr.show(using Printer.TypeReprStructure)}")
          report.errorAndAbort(s"stop reduction: ${repr.show(using Printer.TypeReprStructure)}")
      end match
    end impl

    val l = impl(TypeRepr.of[N])
    (l, path.map(name => ConstantType(StringConstant(name)).asType).toTupleType).runtimeChecked match
      case ('[type l <: Lang2; l], '[type path <: Tuple; path]) =>
        // println((TypeRepr.of[l].show, TypeRepr.of[path].show).toString)
        '{ new Lang2.IsNode.Aux[N, l, path] }
    end match
  end isNodeImpl

  def launderNodeImplRaw[L <: Lang2 : Type](path: List[String])(using Quotes): Type[?] =
    import quotes.reflect.*

    @tailrec
    def impl(path: List[String], base: TypeRepr): TypeRepr =
      path match
        case Nil => report.errorAndAbort(s"Tried to launder into empty path")
        case leaf :: Nil =>
          val sym = base.sym
          val tpSym = sym.typeMember(leaf)
          if tpSym.isNoSymbol
          then report.errorAndAbort(s"Could not find leaf type $leaf on ${base.show(using Printer.TypeReprStructure)}")

          base.select(tpSym)
        case name :: tl =>
          def err: Nothing =
            report.errorAndAbort(s"Could not complete path segment $name on ${base.show(using Printer.TypeReprStructure)}")
          end err

          val sym = base.sym
          val fieldSym = sym.fieldMember(name) match
            case s if s.isNoSymbol =>
              // export forwarders are methods not fields
              sym.methodMember(name) match
                case List(s) => s
                case results => err
              end match
            case s => s
          end fieldSym
          if fieldSym.isNoSymbol
          then err

          impl(tl, base.select(fieldSym))
      end match
    end impl

    // println(s"launder ${TypeRepr.of[L]} :: ${path} ")

    val launderResult = impl(path, TypeRepr.of[L])

    // println(s"--> ${launderResult.show}")

    launderResult.asType
  end launderNodeImplRaw

  def launderNodeImpl[Path <: Tuple : Type, L2 <: Lang2 : Type, N1 : Type](using Quotes): Expr[Lang2.LaunderNode.Aux[L2, N1, ?]] =
    val launderResult = launderNodeImplRaw[L2]:
      Type.of[Path]
        .toList
        .map:
          case '[type name <: String; name] =>
            Type.valueOfConstant[name].get
    end launderResult

    launderResult match
      case '[n2] =>
        '{ new Lang2.LaunderNode.Aux[L2, N1, n2] }
    end match
  end launderNodeImpl

  def hasSameMetaImpl[N1 : Type, N2 : Type, L1 <: Lang2 : Type, L2 <: Lang2 : Type](using Quotes): Expr[Lang2.HasSameMeta[N1, N2]] =
    import quotes.reflect.*

    val l1 = TypeRepr.of[L1]
    val l2 = TypeRepr.of[L2]
    val commonBases = l1
      .baseClasses
      .intersect(l2.baseClasses)
      .filter(_.typeRef <:< TypeRepr.of[Lang2])
      .filterNot(_.typeRef =:= TypeRepr.of[Lang2])
    end commonBases

    if commonBases.isEmpty
    then report.errorAndAbort(s"${ TypeRepr.of[L1].show } and ${ TypeRepr.of[L2].show } have only ${ TypeRepr.of[Lang2].show } as shared base trait")

    if commonBases.size > 2
    then report.errorAndAbort(s"${ TypeRepr.of[L1].show } and ${ TypeRepr.of[L2].show } share multiple Lang2 base traits: ${ commonBases.map(_.typeRef.show).mkString(", ") }")

    '{ new Lang2.HasSameMeta[N1, N2] }
  end hasSameMetaImpl
end MetaMacros
