package dellcliff.html.react

import scala.scalajs.js.Dynamic
import scala.scalajs.js.JSConverters._

sealed case class Component[Props] private(component: Dynamic) {
  def apply(props: Props): Element = {
    Element(Dynamic.global.React.createElement(
      component,
      Dynamic.literal(data = props.asInstanceOf[Dynamic])))
  }
}

sealed trait Node extends NodeOrAttribute

sealed case class Text(text: String) extends Node

sealed case class Element(element: Dynamic) extends Node

object Node {

  private def propsToLiteral(attrs: Traversable[Attribute]): Dynamic = {
    val literal = scala.scalajs.js.Dynamic.literal
    val props = literal()
    attrs.foreach {
      case StringAttribute(key, value) =>
        props.updateDynamic(key)(value)
      case StringListAttribute(key, value) =>
        props.updateDynamic(key)(value.mkString(" "))
      case AttributeList(key, value) =>
        val att = literal()
        value.foreach { case StringAttribute(ikey, ivalue) =>
          att.updateDynamic(ikey)(ivalue)
        }
        props.updateDynamic(key)(att)
      case attr: FuncAttribute =>
        props.updateDynamic(attr.key)(attr.value)
      case other =>
    }
    props
  }

  def element(tag: String, attrs: Traversable[Attribute], nodes: Traversable[Node]): Element = {
    val React = Dynamic.global.React
    val props = propsToLiteral(attrs)
    def unwrap(e: NodeOrAttribute): Traversable[Dynamic] = e match {
      case Element(el) => List(el)
      case Text(text) => List(text.asInstanceOf[Dynamic])
      case ChildNodesOrAttributes(elements) => elements flatMap unwrap
      case other => List()
    }
    val k = nodes flatMap unwrap
    Element(k.size match {
      case 0 => React.createElement(tag, props)
      case 1 => React.createElement(tag, props, k.head)
      case other => React.createElement.bind(null, tag, props).apply(null, k.toJSArray)
    })
  }

  implicit class SymbolOps(val tag: Symbol) extends AnyVal {
    def apply(content: NodeOrAttribute*): Element = tag(content)

    def apply(content: Traversable[NodeOrAttribute]): Element = {
      def attributes(nax: Traversable[NodeOrAttribute]): Traversable[Attribute] =
        nax.flatMap {
          case attr: Attribute => List(attr)
          case ChildNodesOrAttributes(k) => attributes(k)
          case other => List()
        }
      def nodes(nax: Traversable[NodeOrAttribute]): Traversable[Node] =
        nax.flatMap {
          case node: Node => List(node)
          case ChildNodesOrAttributes(k) => nodes(k)
          case other => List()
        }
      element(tag.name, attributes(content), nodes(content))
    }
  }

  implicit class StringOps(val value: String) extends AnyVal {
    def text: Text = Text(value)

    def tag(content: NodeOrAttribute*): Element = tag(content)

    def tag(content: Traversable[NodeOrAttribute]): Element =
      Symbol(value)(content)
  }

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {
    def text(args: Any*): Text = Text(sc.s(args: _*))
  }

  implicit class ElementListOps(val nodes: Traversable[NodeOrAttribute]) extends AnyVal {
    def include: ChildNodesOrAttributes = ChildNodesOrAttributes(nodes)
  }

  implicit class NativeElementOps(val element: Element) extends AnyVal {
    def constant: Element =
      Component(Dynamic.global.React.createClass(Dynamic.literal(
        render = () => element.element,
        shouldComponentUpdate = () => false
      )))(Dynamic.literal())
  }

  implicit class NativeElementFuncOps[Props](val f: Props => Element) extends AnyVal {
    def component: Component[Props] = {
      val render: Dynamic => Dynamic =
        self => f(self.props.data.asInstanceOf[Props]).element

      val shouldComponentUpdate: (Dynamic, Dynamic, Dynamic) => Boolean =
        (self, nextProps, nextState) => self.props.data != nextProps.data

      val literal = scala.scalajs.js.Dynamic.literal

      Component(Dynamic.global.React.createClass(literal(
        render = render: scalajs.js.ThisFunction,
        shouldComponentUpdate = shouldComponentUpdate: scalajs.js.ThisFunction
      )))
    }
  }

}
