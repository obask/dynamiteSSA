package ast

import scala.language.higherKinds


abstract class Context
abstract class Type

abstract class Name {
  /** Is this name a type name? */
  def isTypeName: Boolean

  /** Is this name a term name? */
  def isTermName: Boolean

}

case class TermName(s: String) extends Name {
  def isTypeName = false
  def isTermName = true
}

case class TypeName(s: String) extends Name {
  type ThisName = TypeName
  def isTypeName = true
  def isTermName = false
  def toTypeName = this
  def asTypeName = this

}



case class Signature(paramsSig: List[TypeName], resSig: TypeName)
case class Constant(value: Any)


object Trees {

  // Note: it would be more logical to make Untyped = Nothing.
  // However, this interacts in a bad way with Scala's current type inference.
  // In fact, we cannot write something like Select(pre, name), where pre is
  // of type Tree[Nothing]; type inference will treat the Nothing as an uninstantiated
  // value and will not infer Nothing as the type parameter for Select.
  // We should come back to this issue once type inference is changed.
  type Untyped = Null

  type LazyTree = AnyRef
  /* really: Tree | Lazy[Tree] */
  type LazyTreeList = AnyRef

  /* really: List[Tree] | Lazy[List[Tree]] */

  /** Trees take a parameter indicating what the type of their `tpe` field
    *  is. Two choices: `Type` or `Untyped`.
    * Untyped trees have type `Tree[Untyped]`.
    *
    * Tree typing uses a copy-on-write implementation:
    *
    *   - You can never observe a `tpe` which is `null` (throws an exception)
    *   - So when creating a typed tree with `withType` we can re-use
    * the existing tree transparently, assigning its `tpe` field,
    * provided it was `null` before.
    *   - It is impossible to embed untyped trees in typed ones.
    *   - Typed trees can be embedded untyped ones provided they are rooted
    * in a TypedSplice node.
    *   - Type checking an untyped tree should remove all embedded `TypedSplice`
    * nodes.
    */
  abstract class Tree[-T >: Untyped] extends Product
    with Cloneable {
    /** The type  constructor at the root of the tree */
    type ThisTree[T >: Untyped] <: Tree[T]

    private[this] val myTpe: T = null

    /** Does this tree represent a type? */
    def isType: Boolean = false

    /** Does this tree represent a term? */
    def isTerm: Boolean = false

    /** Is this a legal part of a pattern which is not at the same time a term? */
    def isPattern: Boolean = false

    /** Does this tree define a new symbol that is not defined elsewhere? */
    def isDef: Boolean = false

    /** Is this tree either the empty tree or the empty ValDef? */
    def isEmpty: Boolean = false


    final def hasType: Boolean = myTpe != null

  }

  class UnAssignedTypeException[T >: Untyped](tree: Tree[T]) extends RuntimeException {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  // ------ Categories of trees -----------------------------------

  /** Instances of this class are trees for which isType is definitely true.
    * Note that some trees have isType = true without being TypTrees (e.g. Ident, AnnotatedTree)
    */
  trait TypTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: TypTree[T]
  }

  /** Instances of this class are trees for which isTerm is definitely true.
    * Note that some trees have isTerm = true without being TermTrees (e.g. Ident, AnnotatedTree)
    */
  trait TermTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: TermTree[T]
  }

  /** Instances of this class are trees which are not terms but are legal
    * parts of patterns.
    */
  trait PatternTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: PatternTree[T]
  }

  /** Tree's denotation can be derived from its type */
  abstract class DenotingTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: DenotingTree[T]
  }

  /** Tree's denot/isType/isTerm properties come from a subtree
    * identified by `forwardTo`.
    */
  abstract class ProxyTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: ProxyTree[T]
  }

  /** Tree has a name */
  abstract class NameTree[-T >: Untyped] extends DenotingTree[T] {
    type ThisTree[-T >: Untyped] <: NameTree[T]
  }

  /** Tree refers by name to a denotation */
  abstract class RefTree[-T >: Untyped] extends NameTree[T] {
    type ThisTree[-T >: Untyped] <: RefTree[T]
  }

  /** Tree defines a new symbol */
  trait DefTree[-T >: Untyped] extends DenotingTree[T] {
    type ThisTree[-T >: Untyped] <: DefTree[T]
  }

  val theEmptyTree: Thicket[Type] = Thicket(Nil)
  val theEmptyValDef = new EmptyValDef[Type]

  def genericEmptyValDef[T >: Untyped]: ValDef[T] = theEmptyValDef.asInstanceOf[ValDef[T]]

  def genericEmptyTree[T >: Untyped]: Thicket[T] = theEmptyTree.asInstanceOf[Thicket[T]]


  /** Tree defines a new symbol and carries modifiers.
    * The position of a MemberDef contains only the defined identifier or pattern.
    * The envelope of a MemberDef contains the whole definition and has its point
    * on the opening keyword (or the next token after that if keyword is missing).
    */
  abstract class MemberDef[-T >: Untyped] extends NameTree[T] with DefTree[T] {
    type ThisTree[-T >: Untyped] <: MemberDef[T]
  }

  /** A ValDef or DefDef tree */
  trait ValOrDefDef[-T >: Untyped] extends MemberDef[T] with WithLazyField[Tree[T]] {
    def tpt: Tree[T]

    def unforcedRhs: LazyTree = unforced

    def rhs(implicit ctx: Context): Tree[T] = forceIfLazy
  }

  // ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[-T >: Untyped] private[ast](name: Name)
    extends RefTree[T] {
    type ThisTree[-T >: Untyped] = Ident[T]
  }

  class BackquotedIdent[-T >: Untyped] private[ast](name: Name)
    extends Ident[T](name) {
    override def toString = s"BackquotedIdent($name)"
  }

  /** qualifier.name */
  case class Select[-T >: Untyped] private[ast](qualifier: Tree[T], name: Name)
    extends RefTree[T] {
    type ThisTree[-T >: Untyped] = Select[T]
  }

  class SelectWithSig[-T >: Untyped] private[ast](qualifier: Tree[T], name: Name, val sig: Signature)
    extends Select[T](qualifier, name) {
    override def toString = s"SelectWithSig($qualifier, $name, $sig)"
  }

  /** qual.this */
  case class This[-T >: Untyped] private[ast](qual: TypeName)
    extends DenotingTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] = This[T]
  }

  /** C.super[mix], where qual = C.this */
  case class Super[-T >: Untyped] private[ast](qual: Tree[T], mix: TypeName)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] = Super[T]

    def forwardTo = qual
  }

  abstract class GenericApply[-T >: Untyped] extends ProxyTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] <: GenericApply[T]
    val fun: Tree[T]
    val args: List[Tree[T]]

    def forwardTo = fun
  }

  /** fun(args) */
  case class Apply[-T >: Untyped] private[ast](fun: Tree[T], args: List[Tree[T]])
    extends GenericApply[T] {
    type ThisTree[-T >: Untyped] = Apply[T]
  }

  /** fun[args] */
  case class TypeApply[-T >: Untyped] private[ast](fun: Tree[T], args: List[Tree[T]])
    extends GenericApply[T] {
    type ThisTree[-T >: Untyped] = TypeApply[T]
  }

  /** const */
  case class Literal[-T >: Untyped] private[ast](const: Constant)
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Literal[T]
  }

  /** new tpt, but no constructor call */
  case class New[-T >: Untyped] private[ast](tpt: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = New[T]
  }

  /** (left, right) */
  case class Pair[-T >: Untyped] private[ast](left: Tree[T], right: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Pair[T]

    override def isTerm = left.isTerm && right.isTerm

    override def isType = left.isType && right.isType

    override def isPattern = !isTerm && (left.isPattern || left.isTerm) && (right.isPattern || right.isTerm)
  }

  /** expr : tpt */
  case class Typed[-T >: Untyped] private[ast](expr: Tree[T], tpt: Tree[T])
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] = Typed[T]

    def forwardTo = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg[-T >: Untyped] private[ast](name: Name, arg: Tree[T])
    extends Tree[T] {
    type ThisTree[-T >: Untyped] = NamedArg[T]
  }

  /** name = arg, outside a parameter list */
  case class Assign[-T >: Untyped] private[ast](lhs: Tree[T], rhs: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Assign[T]
  }

  /** { stats; expr } */
  case class Block[-T >: Untyped] private[ast](stats: List[Tree[T]], expr: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Block[T]
  }

  /** if cond then thenp else elsep */
  case class If[-T >: Untyped] private[ast](cond: Tree[T], thenp: Tree[T], elsep: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = If[T]
  }

  /** A closure with an environment and a reference to a method.
    *
    * @param env  The captured parameters of the closure
    * @param meth A ref tree that refers to the method of the closure.
    *             The first (env.length) parameters of that method are filled
    *             with env values.
    * @param tpt  Either EmptyTree or a TypeTree. If tpt is EmptyTree the type
    *             of the closure is a function type, otherwise it is the type
    *             given in `tpt`, which must be a SAM type.
    */
  case class Closure[-T >: Untyped] private[ast](env: List[Tree[T]], meth: Tree[T], tpt: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Closure[T]
  }

  /** selector match { cases } */
  case class Match[-T >: Untyped] private[ast](selector: Tree[T], cases: List[CaseDef[T]])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Match[T]
  }

  /** case pat if guard => body; only appears as child of a Match */
  case class CaseDef[-T >: Untyped] private[ast](pat: Tree[T], guard: Tree[T], body: Tree[T])
    extends Tree[T] {
    type ThisTree[-T >: Untyped] = CaseDef[T]
  }

  /** return expr
    * where `from` refers to the method from which the return takes place
    * After program transformations this is not necessarily the enclosing method, because
    * closures can intervene.
    */
  case class Return[-T >: Untyped] private[ast](expr: Tree[T], from: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Return[T]
  }

  /** try block catch handler finally finalizer
    *
    * Note: if the handler is a case block CASES of the form
    *
    * { case1 ... caseN }
    *
    * the parser returns Match(EmptyTree, CASES). Desugaring and typing this yields a closure
    * node
    *
    * { def $anonfun(x: Throwable) = x match CASES; Closure(Nil, $anonfun) }
    *
    * At some later stage when we normalize the try we can revert this to
    *
    * Match(EmptyTree, CASES)
    *
    * or else if stack is non-empty
    *
    * Match(EmptyTree, <case x: Throwable => $anonfun(x)>)
    */
  case class Try[-T >: Untyped] private[ast](expr: Tree[T], cases: List[CaseDef[T]], finalizer: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Try[T]
  }

  /** Seq(elems)
    */
  case class SeqLiteral[-T >: Untyped] private[ast](elems: List[Tree[T]], elemtpt: Tree[T])
    extends Tree[T] {
    type ThisTree[-T >: Untyped] = SeqLiteral[T]
  }

  /** Array(elems) */
  class JavaSeqLiteral[T >: Untyped] private[ast](elems: List[Tree[T]], elemtpt: Tree[T])
    extends SeqLiteral(elems, elemtpt) {
    override def toString = s"JavaSeqLiteral($elems, $elemtpt)"
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree[-T >: Untyped] private[ast](original: Tree[T])
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = TypeTree[T]

    override def isEmpty = !hasType && original.isEmpty
  }

  /** ref.type */
  case class SingletonTypeTree[-T >: Untyped] private[ast](ref: Tree[T])
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = SingletonTypeTree[T]
  }

  /** qualifier # name
    * In Scala, this always refers to a type, but in a Java
    * compilation unit this might refer to a term.
    */
  case class SelectFromTypeTree[-T >: Untyped] private[ast](qualifier: Tree[T], name: Name)
    extends RefTree[T] {
    type ThisTree[-T >: Untyped] = SelectFromTypeTree[T]
  }

  /** left & right */
  case class AndTypeTree[-T >: Untyped] private[ast](left: Tree[T], right: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = AndTypeTree[T]
  }

  /** left | right */
  case class OrTypeTree[-T >: Untyped] private[ast](left: Tree[T], right: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = OrTypeTree[T]
  }

  /** tpt { refinements } */
  case class RefinedTypeTree[-T >: Untyped] private[ast](tpt: Tree[T], refinements: List[Tree[T]])
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = RefinedTypeTree[T]

    def forwardTo = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree[-T >: Untyped] private[ast](tpt: Tree[T], args: List[Tree[T]])
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = AppliedTypeTree[T]

    def forwardTo = tpt
  }

  /** [typeparams] -> tpt */
  case class TypeLambdaTree[-T >: Untyped] private[ast](tparams: List[TypeDef[T]], body: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = TypeLambdaTree[T]
  }

  /** => T */
  case class ByNameTypeTree[-T >: Untyped] private[ast](result: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = ByNameTypeTree[T]
  }

  /** >: lo <: hi */
  case class TypeBoundsTree[-T >: Untyped] private[ast](lo: Tree[T], hi: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = TypeBoundsTree[T]
  }

  /** name @ body */
  case class Bind[-T >: Untyped] private[ast](name: Name, body: Tree[T])
    extends NameTree[T] with DefTree[T] with PatternTree[T] {
    type ThisTree[-T >: Untyped] = Bind[T]

    override def isType = name.isTypeName

    override def isTerm = name.isTermName
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[-T >: Untyped] private[ast](trees: List[Tree[T]])
    extends PatternTree[T] {
    type ThisTree[-T >: Untyped] = Alternative[T]
  }

  /** The typed translation of `extractor(patterns)` in a pattern. The translation has the following
    * components:
    *
    * @param fun       is `extractor.unapply` (or, for backwards compatibility, `extractor.unapplySeq`)
    *                  possibly with type parameters
    * @param implicits Any implicit parameters passed to the unapply after the selector
    * @param patterns  The argument patterns in the pattern match.
    *
    *                  It is typed with same type as first `fun` argument
    *                  Given a match selector `sel` a pattern UnApply(fun, implicits, patterns) is roughly translated as follows
    *
    *                  val result = fun(sel)(implicits)
    *                  if (result.isDefined) "match patterns against result"
    */
  case class UnApply[-T >: Untyped] private[ast](fun: Tree[T], implicits: List[Tree[T]], patterns: List[Tree[T]])
    extends PatternTree[T] {
    type ThisTree[-T >: Untyped] = UnApply[T]
  }

  /** mods val name: tpt = rhs */
  case class ValDef[-T >: Untyped] private[ast](name: TermName, tpt: Tree[T], private var preRhs: LazyTree)
    extends ValOrDefDef[T] {
    type ThisTree[-T >: Untyped] = ValDef[T]

    assert(isEmpty || tpt != genericEmptyTree)

    def unforced = preRhs

    protected def force(x: AnyRef) = preRhs = x

  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[-T >: Untyped] private[ast](name: TermName, tparams: List[TypeDef[T]],
                                                vparamss: List[List[ValDef[T]]], tpt: Tree[T], private var preRhs: LazyTree)
    extends ValOrDefDef[T] {
    type ThisTree[-T >: Untyped] = DefDef[T]
    assert(tpt != genericEmptyTree)

    def unforced = preRhs

    protected def force(x: AnyRef) = preRhs = x
  }

  /** mods class name template     or
    * mods trait name template     or
    * mods type name = rhs   or
    * mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi) & (lo ne hi)
    */
  case class TypeDef[-T >: Untyped] private[ast](name: TypeName, rhs: Tree[T])
    extends MemberDef[T] {
    type ThisTree[-T >: Untyped] = TypeDef[T]

    /** Is this a definition of a class? */
    def isClassDef = rhs.isInstanceOf[Template[_]]

  }

  /** extends parents { self => body } */
  case class Template[-T >: Untyped] private[ast](constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], private var preBody: LazyTreeList)
    extends DefTree[T] with WithLazyField[List[Tree[T]]] {
    type ThisTree[-T >: Untyped] = Template[T]

    def unforcedBody = unforced

    def unforced = preBody

    protected def force(x: AnyRef) = preBody = x

    def body(implicit ctx: Context): List[Tree[T]] = forceIfLazy
  }

  /** import expr.selectors
    * where a selector is either an untyped `Ident`, `name` or
    * an untyped `Pair` `name => rename`
    */
  case class Import[-T >: Untyped] private[ast](expr: Tree[T], selectors: List[Tree[Untyped]])
    extends DenotingTree[T] {
    type ThisTree[-T >: Untyped] = Import[T]
  }

  /** package pid { stats } */
  case class PackageDef[-T >: Untyped] private[ast](pid: RefTree[T], stats: List[Tree[T]])
    extends ProxyTree[T] {
    type ThisTree[-T >: Untyped] = PackageDef[T]

    def forwardTo = pid
  }

  /** arg @annot */
  case class Annotated[-T >: Untyped] private[ast](annot: Tree[T], arg: Tree[T])
    extends ProxyTree[T] {
    type ThisTree[-T >: Untyped] = Annotated[T]

    def forwardTo = arg
  }

  trait WithoutTypeOrPos[-T >: Untyped] extends Tree[T]

  /** Temporary class that results from translation of ModuleDefs
    * (and possibly other statements).
    * The contained trees will be integrated when transformed with
    * a `transform(List[Tree])` call.
    */
  case class Thicket[-T >: Untyped](trees: List[Tree[T]])
    extends Tree[T] with WithoutTypeOrPos[T] {
    type ThisTree[-T >: Untyped] = Thicket[T]

    override def isEmpty: Boolean = trees.isEmpty
  }


  class EmptyValDef[T >: Untyped] extends ValDef[T](
    null, genericEmptyTree[T], genericEmptyTree[T]) with WithoutTypeOrPos[T] {
    override def isEmpty: Boolean = true
  }

  // ----- Lazy trees and tree sequences

  /** A tree that can have a lazy field
    * The field is represented by some private `var` which is
    * proxied `unforced` and `force`. Forcing the field will
    * set the `var` to the underlying value.
    */
  trait WithLazyField[+T <: AnyRef] {
    def unforced: AnyRef

    protected def force(x: AnyRef): Unit

    def forceIfLazy(implicit ctx: Context): T = unforced match {
      case lzy: Lazy[T] =>
        val x = lzy.complete
        force(x)
        x
      case x: T@unchecked => x
    }
  }

  /** A base trait for lazy tree fields.
    * These can be instantiated with Lazy instances which
    * can delay tree construction until the field is first demanded.
    */
  trait Lazy[T <: AnyRef] {
    def complete(implicit ctx: Context): T
  }

  // ----- Generic Tree Instances, inherited from  `tpt` and `untpd`.

  abstract class Instance[T >: Untyped <: Type] {
    inst =>

    type Tree = Trees.Tree[T]
    type TypTree = Trees.TypTree[T]
    type TermTree = Trees.TermTree[T]
    type PatternTree = Trees.PatternTree[T]
    type DenotingTree = Trees.DenotingTree[T]
    type ProxyTree = Trees.ProxyTree[T]
    type NameTree = Trees.NameTree[T]
    type RefTree = Trees.RefTree[T]
    type DefTree = Trees.DefTree[T]
    type MemberDef = Trees.MemberDef[T]
    type ValOrDefDef = Trees.ValOrDefDef[T]

    type Ident = Trees.Ident[T]
    type BackquotedIdent = Trees.BackquotedIdent[T]
    type Select = Trees.Select[T]
    type SelectWithSig = Trees.SelectWithSig[T]
    type This = Trees.This[T]
    type Super = Trees.Super[T]
    type Apply = Trees.Apply[T]
    type TypeApply = Trees.TypeApply[T]
    type Literal = Trees.Literal[T]
    type New = Trees.New[T]
    type Pair = Trees.Pair[T]
    type Typed = Trees.Typed[T]
    type NamedArg = Trees.NamedArg[T]
    type Assign = Trees.Assign[T]
    type Block = Trees.Block[T]
    type If = Trees.If[T]
    type Closure = Trees.Closure[T]
    type Match = Trees.Match[T]
    type CaseDef = Trees.CaseDef[T]
    type Return = Trees.Return[T]
    type Try = Trees.Try[T]
    type SeqLiteral = Trees.SeqLiteral[T]
    type JavaSeqLiteral = Trees.JavaSeqLiteral[T]
    type TypeTree = Trees.TypeTree[T]
    type SingletonTypeTree = Trees.SingletonTypeTree[T]
    type SelectFromTypeTree = Trees.SelectFromTypeTree[T]
    type AndTypeTree = Trees.AndTypeTree[T]
    type OrTypeTree = Trees.OrTypeTree[T]
    type RefinedTypeTree = Trees.RefinedTypeTree[T]
    type AppliedTypeTree = Trees.AppliedTypeTree[T]
    type TypeLambdaTree = Trees.TypeLambdaTree[T]
    type ByNameTypeTree = Trees.ByNameTypeTree[T]
    type TypeBoundsTree = Trees.TypeBoundsTree[T]
    type Bind = Trees.Bind[T]
    type Alternative = Trees.Alternative[T]
    type UnApply = Trees.UnApply[T]
    type ValDef = Trees.ValDef[T]
    type DefDef = Trees.DefDef[T]
    type TypeDef = Trees.TypeDef[T]
    type Template = Trees.Template[T]
    type Import = Trees.Import[T]
    type PackageDef = Trees.PackageDef[T]
    type Annotated = Trees.Annotated[T]
    type Thicket = Trees.Thicket[T]

    val EmptyTree: Thicket = genericEmptyTree
    val EmptyValDef: ValDef = genericEmptyValDef

    // ----- Auxiliary creation methods ------------------

    def Thicket(trees: List[Tree]): Thicket = new Thicket(trees)

    def Thicket(): Thicket = EmptyTree

    def Thicket(x1: Tree, x2: Tree): Thicket = Thicket(x1 :: x2 :: Nil)

    def Thicket(x1: Tree, x2: Tree, x3: Tree): Thicket = Thicket(x1 :: x2 :: x3 :: Nil)
  }

}

