package core

import ast.Trees.Tree
import ast.{Constant, Name, TermName, TypeName}


object Types {


  /** The class of types.
    * The principal subclasses and sub-objects are as follows:
    *
    * Type -+- ProxyType --+- NamedType ----+--- TypeRef
    * |              |                 \
    * |              +- SingletonType-+-+- TermRef
    * |              |                |
    * |              |                +--- ThisType
    * |              |                +--- SuperType
    * |              |                +--- ConstantType
    * |              |                +--- MethodParam
    * |              |                +----RecThis
    * |              |                +--- SkolemType
    * |              +- PolyParam
    * |              +- RefinedOrRecType -+-- RefinedType
    * |              |                   -+-- RecType
    * |              +- HKApply
    * |              +- TypeBounds
    * |              +- ExprType
    * |              +- AnnotatedType
    * |              +- TypeVar
    * |
    * +- GroundType -+- AndType
    * +- OrType
    * +- MethodType -----+- ImplicitMethodType
    * |                  +- JavaMethodType
    * +- ClassInfo
    * +- GenericType ----+- PolyType
    * |                  +- TypeLambda
    * |
    * +- NoType
    * +- NoPrefix
    * +- ErrorType
    * +- WildcardType
    */

  trait Type
  abstract class Context

//  case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType


  case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {
    def variance: Int = 0
  }

  abstract class Annotation

  abstract class TypeProxy extends Type
  abstract class CachedGroundType extends Type with CachedType
  abstract class CachedProxyType extends TypeProxy with CachedType
  abstract class UncachedGroundType extends Type
  abstract class UncachedProxyType extends TypeProxy
  abstract class NamedType extends CachedProxyType with ValueType {
    val prefix: Type
    val name: Name
  }
  abstract class RefinedOrRecType extends CachedProxyType with ValueType
  abstract class MethodTypeCompanion
  abstract class BoundType extends CachedProxyType with ValueType
  abstract class ParamType extends BoundType
  abstract class TypeAlias(val alias: Type, override val variance: Int) extends TypeBounds(alias, alias)
  abstract class ErrorType extends UncachedGroundType with ValueType
  abstract class TypeMap(implicit protected val ctx: Context) extends (Type => Type)
  abstract class DeepTypeMap(implicit ctx: Context) extends TypeMap
  abstract class ApproximatingTypeMap(implicit ctx: Context) extends TypeMap
  abstract class TypeAccumulator[T](implicit protected val ctx: Context)
  abstract class TypeTraverser(implicit ctx: Context) extends TypeAccumulator[Unit]
  abstract class NameFilter

  trait CachedType extends Type
  trait TypeType extends Type
  trait TermType extends Type
  trait ValueTypeOrProto extends TermType
  trait ValueType extends ValueTypeOrProto
  trait SingletonType extends TypeProxy with ValueType
  trait BindingType extends Type
  trait ProtoType extends Type
  trait NarrowCached extends Type
  trait WithFixedSym extends NamedType
  trait AndOrType extends ValueType
  trait MethodicType extends Type
  trait MethodOrPoly extends MethodicType
  trait GenericType extends BindingType with TermType

  trait TypeParamInfo


  case class TermRef(override val prefix: Type, name: TermName) extends NamedType with SingletonType
  case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType
  case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType
  case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType
  case class ConstantType(value: Constant) extends CachedProxyType with SingletonType
  case class LazyRef(refFn: () => Type) extends UncachedProxyType with ValueType
  case class RefinedType(parent: Type, refinedName: Name, refinedInfo: Type) extends RefinedOrRecType
  case class AndType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType
  case class OrType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType
  case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
  case class ExprType(resType: Type)
  case class LambdaParam(tl: TypeLambda, n: Int) extends TypeParamInfo
  case class HKApply(tycon: Type, args: List[Type])
  case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType
  case class PolyParam(binder: GenericType, paramNum: Int) extends ParamType
  case class SkolemType(info: Type) extends UncachedProxyType with ValueType with SingletonType
  case class AnnotatedType(tpe: Type, annot: Annotation)
  case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType
//  case class ImportType(expr: Tree) extends UncachedGroundType
  case class WildcardType(optBounds: Type) extends CachedGroundType with TermType

  class TypeLambda(val paramNames: List[TypeName], val variances: List[Int])(paramBoundsExp: GenericType => List[TypeBounds], resultTypeExp: GenericType => Type)
    extends CachedProxyType with GenericType with ValueType

  case object NoType extends CachedGroundType
  case object NoPrefix extends CachedGroundType

}