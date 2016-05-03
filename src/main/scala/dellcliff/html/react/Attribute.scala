package dellcliff.html.react

sealed trait Attribute extends NodeOrAttribute

sealed case class StringAttribute
(key: String, value: String) extends Attribute

sealed case class StringListAttribute
(key: String, value: Traversable[String]) extends Attribute

sealed case class FuncAttribute
(key: String, value: Any => _) extends Attribute

sealed case class AttributeList
(key: String, value: Traversable[StringAttribute]) extends Attribute


object Attribute {

  implicit class AttributeStringOps(val key: String) extends AnyVal {
    def :=(value: String) = StringAttribute(key, value)

    def :=(value: String*) = StringListAttribute(key, value)

    def :=(value: Any => _) = FuncAttribute(key, value)

    def :=(value: () => _) = FuncAttribute(key, { x: Any => value() })

    def :=(value: StringAttribute*) = AttributeList(key, value)

    def :=(value: PartialFunction[Any, Any]) = FuncAttribute(key, value orElse { case other => })

    def :-(value: PartialFunction[Any, Any]) = FuncAttribute(key, value orElse { case other => })
  }

  implicit class AttributeSymbolOps(val key: Symbol) extends AnyVal {
    def :=(value: String) = StringAttribute(key.name, value)

    def :=(value: String*) = StringListAttribute(key.name, value)

    def :=(value: Any => _) = FuncAttribute(key.name, value)

    def :=(value: () => _) = FuncAttribute(key.name, { x: Any => value() })

    def :=(value: StringAttribute*) = AttributeList(key.name, value)

    def :=(value: PartialFunction[Any, Any]) = FuncAttribute(key.name, value orElse { case other => })

    def :-(value: PartialFunction[Any, Any]) = FuncAttribute(key.name, value orElse { case other => })
  }

}
