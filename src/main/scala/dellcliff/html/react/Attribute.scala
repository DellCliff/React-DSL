package dellcliff.html.react

sealed trait Attribute extends NodeOrAttribute {
  val key: String
}

sealed case class StringAttribute
(key: String, value: String) extends Attribute

sealed case class StringListAttribute
(key: String, value: Traversable[String]) extends Attribute

sealed case class FuncAttribute
(key: String, value: Any => _) extends Attribute

sealed case class AttributeList
(key: String, value: Traversable[StringAttribute]) extends Attribute


object Attribute {
  type Attributes = Traversable[Attribute]

  def attribute(key: String, value: String) = StringAttribute(key, value)

  def attribute(key: String, value: String*) = StringListAttribute(key, value)

  def partialFuncAttr(key: String, value: PartialFunction[Any, _]) = FuncAttribute(key, value orElse { case other => })

  def attribute(key: String, value: PartialFunction[Any, _]) = partialFuncAttr(key, value)

  def attribute(key: String, value: Any => _) = FuncAttribute(key, value)

  def attribute(key: String, value: () => _) = FuncAttribute(key, { x: Any => value() })

  def attribute(key: String, value: StringAttribute*) = AttributeList(key, value)

  implicit class AttributeStringOps(val key: String) extends AnyVal {
    def :=(value: String) = Attribute.attribute(key, value)

    def :=(value: String*) = Attribute.attribute(key, value: _*)

    def :=(value: Any => _) = Attribute.attribute(key, value)

    def :=(value: () => _) = Attribute.attribute(key, value)

    def :=(value: StringAttribute*) = Attribute.attribute(key, value: _*)

    def :=(value: PartialFunction[Any, Any]) = Attribute.attribute(key, value)

    def :-(value: PartialFunction[Any, Any]) = Attribute.attribute(key, value)
  }

  implicit class AttributeSymbolOps(val key: Symbol) extends AnyVal {
    def :=(value: String) = Attribute.attribute(key.name, value)

    def :=(value: String*) = Attribute.attribute(key.name, value: _*)

    def :=(value: Any => _) = Attribute.attribute(key.name, value)

    def :=(value: () => _) = Attribute.attribute(key.name, value)

    def :=(value: StringAttribute*) = Attribute.attribute(key.name, value: _*)

    def :=(value: PartialFunction[Any, Any]) = Attribute.attribute(key.name, value)

    def :-(value: PartialFunction[Any, Any]) = Attribute.attribute(key.name, value)
  }

}
