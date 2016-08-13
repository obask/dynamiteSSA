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
  abstract class Tree extends Product
    with Cloneable {
    /** The type  constructor at the root of the tree */
    type ThisTree <: Tree

    private[this] val myTpe = null

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

  class UnAssignedTypeException[T >: Untyped](tree: Tree) extends RuntimeException {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  // ------ Categories of trees -----------------------------------

  /** Instances of this class are trees for which isType is definitely true.
    * Note that some trees have isType = true without being TypTrees (e.g. Ident, AnnotatedTree)
    */
  trait TypTree extends Tree {
  }

  /** Instances of this class are trees for which isTerm is definitely true.
    * Note that some trees have isTerm = true without being TermTrees (e.g. Ident, AnnotatedTree)
    */
  trait TermTree extends Tree {
  }

  /** Instances of this class are trees which are not terms but are legal
    * parts of patterns.
    */
  trait PatternTree extends Tree {
  }

  /** Tree's denotation can be derived from its type */
  abstract class DenotingTree extends Tree {
  }

  /** Tree's denot/isType/isTerm properties come from a subtree
    * identified by `forwardTo`.
    */
  abstract class ProxyTree extends Tree {
  }

  /** Tree has a name */
  abstract class NameTree extends DenotingTree {
  }

  /** Tree refers by name to a denotation */
  abstract class RefTree extends NameTree {
  }

  /** Tree defines a new symbol */
  trait DefTree extends DenotingTree {
  }

  val theEmptyTree: Thicket = Thicket(Nil)
  val theEmptyValDef = new EmptyValDef

  def genericEmptyValDef[T >: Untyped]: ValDef = theEmptyValDef.asInstanceOf[ValDef]

  def genericEmptyTree[T >: Untyped]: Thicket = theEmptyTree.asInstanceOf[Thicket]


  /** Tree defines a new symbol and carries modifiers.
    * The position of a MemberDef contains only the defined identifier or pattern.
    * The envelope of a MemberDef contains the whole definition and has its point
    * on the opening keyword (or the next token after that if keyword is missing).
    */
  abstract class MemberDef extends NameTree with DefTree {
  }

  /** A ValDef or DefDef tree */
  trait ValOrDefDef extends MemberDef with WithLazyField {
    def tpt: Tree

    def unforcedRhs: LazyTree = unforced

    def rhs(implicit ctx: Context): Tree = unforced.asInstanceOf[Tree]
  }

  // ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident private[ast](name: Name)
    extends RefTree {
  }

  class BackquotedIdent private[ast](name: Name)
    extends Ident(name) {
    override def toString = s"BackquotedIdent($name)"
  }

  /** qualifier.name */
  case class Select private[ast](qualifier: Tree, name: Name)
    extends RefTree {
  }

  class SelectWithSig private[ast](qualifier: Tree, name: Name, val sig: Signature)
    extends Select(qualifier, name) {
    override def toString = s"SelectWithSig($qualifier, $name, $sig)"
  }

  /** qual.this */
  case class This private[ast](qual: TypeName)
    extends DenotingTree with TermTree {
  }

  /** C.super[mix], where qual = C.this */
  case class Super private[ast](qual: Tree, mix: TypeName)
    extends ProxyTree with TermTree {

    def forwardTo = qual
  }

  abstract class GenericApply extends ProxyTree with TermTree {
    type ThisTree <: GenericApply
    val fun: Tree
    val args: List[Tree]

    def forwardTo = fun
  }

  /** fun(args) */
  case class Apply private[ast](fun: Tree, args: List[Tree])
    extends GenericApply {
    type ThisTree = Apply
  }

  /** fun[args] */
  case class TypeApply private[ast](fun: Tree, args: List[Tree])
    extends GenericApply {
  }

  /** const */
  case class Literal private[ast](const: Constant)
    extends TermTree {
  }

  /** new tpt, but no constructor call */
  case class New private[ast](tpt: Tree)
    extends TermTree {
  }

  /** (left, right) */
  case class Pair private[ast](left: Tree, right: Tree)
    extends TermTree {

    override def isTerm = left.isTerm && right.isTerm

    override def isType = left.isType && right.isType

    override def isPattern = !isTerm && (left.isPattern || left.isTerm) && (right.isPattern || right.isTerm)
  }

  /** expr : tpt */
  case class Typed private[ast](expr: Tree, tpt: Tree)
    extends ProxyTree with TermTree {

    def forwardTo = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg private[ast](name: Name, arg: Tree)
    extends Tree {
  }

  /** name = arg, outside a parameter list */
  case class Assign private[ast](lhs: Tree, rhs: Tree)
    extends TermTree {
  }

  /** { stats; expr } */
  case class Block private[ast](stats: List[Tree], expr: Tree)
    extends TermTree {
  }

  /** if cond then thenp else elsep */
  case class If private[ast](cond: Tree, thenp: Tree, elsep: Tree)
    extends TermTree {
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
  case class Closure private[ast](env: List[Tree], meth: Tree, tpt: Tree)
    extends TermTree {
  }

  /** selector match { cases } */
  case class Match private[ast](selector: Tree, cases: List[CaseDef])
    extends TermTree {
  }

  /** case pat if guard => body; only appears as child of a Match */
  case class CaseDef private[ast](pat: Tree, guard: Tree, body: Tree)
    extends Tree {
  }

  /** return expr
    * where `from` refers to the method from which the return takes place
    * After program transformations this is not necessarily the enclosing method, because
    * closures can intervene.
    */
  case class Return private[ast](expr: Tree, from: Tree)
    extends TermTree {
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
  case class Try private[ast](expr: Tree, cases: List[CaseDef], finalizer: Tree)
    extends TermTree {
    type ThisTree = Try
  }

  /** Seq(elems)
    */
  case class SeqLiteral private[ast](elems: List[Tree], elemtpt: Tree)
    extends Tree {
    type ThisTree = SeqLiteral
  }

  /** Array(elems) */
  class JavaSeqLiteral[T >: Untyped] private[ast](elems: List[Tree], elemtpt: Tree)
    extends SeqLiteral(elems, elemtpt) {
    override def toString = s"JavaSeqLiteral($elems, $elemtpt)"
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree private[ast](original: Tree)
    extends DenotingTree with TypTree {
    type ThisTree = TypeTree

    override def isEmpty = !hasType && original.isEmpty
  }

  /** ref.type */
  case class SingletonTypeTree private[ast](ref: Tree)
    extends DenotingTree with TypTree {
    type ThisTree = SingletonTypeTree
  }

  /** qualifier # name
    * In Scala, this always refers to a type, but in a Java
    * compilation unit this might refer to a term.
    */
  case class SelectFromTypeTree private[ast](qualifier: Tree, name: Name)
    extends RefTree {
    type ThisTree = SelectFromTypeTree
  }

  /** left & right */
  case class AndTypeTree private[ast](left: Tree, right: Tree)
    extends TypTree {
    type ThisTree = AndTypeTree
  }

  /** left | right */
  case class OrTypeTree private[ast](left: Tree, right: Tree)
    extends TypTree {
    type ThisTree = OrTypeTree
  }

  /** tpt { refinements } */
  case class RefinedTypeTree private[ast](tpt: Tree, refinements: List[Tree])
    extends ProxyTree with TypTree {
    type ThisTree = RefinedTypeTree

    def forwardTo = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree private[ast](tpt: Tree, args: List[Tree])
    extends ProxyTree with TypTree {
    type ThisTree = AppliedTypeTree

    def forwardTo = tpt
  }

  /** [typeparams] -> tpt */
  case class TypeLambdaTree private[ast](tparams: List[TypeDef], body: Tree)
    extends TypTree {
    type ThisTree = TypeLambdaTree
  }

  /** => T */
  case class ByNameTypeTree private[ast](result: Tree)
    extends TypTree {
    type ThisTree = ByNameTypeTree
  }

  /** >: lo <: hi */
  case class TypeBoundsTree private[ast](lo: Tree, hi: Tree)
    extends TypTree {
    type ThisTree = TypeBoundsTree
  }

  /** name @ body */
  case class Bind private[ast](name: Name, body: Tree)
    extends NameTree with DefTree with PatternTree {
    type ThisTree = Bind

    override def isType = name.isTypeName

    override def isTerm = name.isTermName
  }

  /** tree_1 | ... | tree_n */
  case class Alternative private[ast](trees: List[Tree])
    extends PatternTree {
    type ThisTree = Alternative
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
  case class UnApply private[ast](fun: Tree, implicits: List[Tree], patterns: List[Tree])
    extends PatternTree {
    type ThisTree = UnApply
  }

  /** mods val name: tpt = rhs */
  case class ValDef private[ast](name: TermName, tpt: Tree, private var preRhs: LazyTree)
    extends ValOrDefDef {
    type ThisTree = ValDef

    assert(isEmpty || tpt != genericEmptyTree)

    def unforced = preRhs

  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef private[ast](name: TermName, tparams: List[TypeDef],
                                                vparamss: List[List[ValDef]], tpt: Tree, private var preRhs: LazyTree)
    extends ValOrDefDef {
    type ThisTree = DefDef
    assert(tpt != genericEmptyTree)

    def unforced = preRhs

  }

  /** mods class name template     or
    * mods trait name template     or
    * mods type name = rhs   or
    * mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi) & (lo ne hi)
    */
  case class TypeDef private[ast](name: TypeName, rhs: Tree)
    extends MemberDef {
    type ThisTree = TypeDef

    /** Is this a definition of a class? */
    def isClassDef = rhs.isInstanceOf[Template]

  }

  /** extends parents { self => body } */
  case class Template private[ast](constr: DefDef, parents: List[Tree], self: ValDef, private var preBody: LazyTreeList)
    extends DefTree with WithLazyField {
    type ThisTree = Template

    def unforcedBody = unforced

    def unforced = preBody

    protected def force(x: AnyRef) = preBody = x

  }

  /** import expr.selectors
    * where a selector is either an untyped `Ident`, `name` or
    * an untyped `Pair` `name => rename`
    */
  case class Import private[ast](expr: Tree, selectors: List[Tree])
    extends DenotingTree {
    type ThisTree = Import
  }

  /** package pid { stats } */
  case class PackageDef private[ast](pid: RefTree, stats: List[Tree])
    extends ProxyTree {
    type ThisTree = PackageDef

    def forwardTo = pid
  }

  /** arg @annot */
  case class Annotated private[ast](annot: Tree, arg: Tree)
    extends ProxyTree {
    type ThisTree = Annotated

    def forwardTo = arg
  }

  trait WithoutTypeOrPos extends Tree

  /** Temporary class that results from translation of ModuleDefs
    * (and possibly other statements).
    * The contained trees will be integrated when transformed with
    * a `transform(List[Tree])` call.
    */
  case class Thicket(trees: List[Tree])
    extends Tree with WithoutTypeOrPos {
    type ThisTree = Thicket

    override def isEmpty: Boolean = trees.isEmpty
  }


  class EmptyValDef extends ValDef(
    null, genericEmptyTree, genericEmptyTree) with WithoutTypeOrPos {
    override def isEmpty: Boolean = true

  }

  // ----- Lazy trees and tree sequences

  /** A tree that can have a lazy field
    * The field is represented by some private `var` which is
    * proxied `unforced` and `force`. Forcing the field will
    * set the `var` to the underlying value.
    */
  trait WithLazyField {
    def unforced: AnyRef

  }

  /** A base trait for lazy tree fields.
    * These can be instantiated with Lazy instances which
    * can delay tree construction until the field is first demanded.
    */
  trait Lazy {
    def complete(implicit ctx: Context)
  }



}

