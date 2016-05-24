package dellcliff.html.react

import scala.scalajs.js.Dynamic
import scala.scalajs.js.JSConverters._
import scala.language.implicitConversions
import scala.util.Try


trait NodeOrAttribute

case class ChildNodesOrAttributes(nodesOrAttributes: Traversable[NodeOrAttribute]) extends NodeOrAttribute

sealed trait Node extends NodeOrAttribute

sealed case class Text(text: String) extends Node

sealed case class Element(element: Try[Dynamic]) extends Node

object Node {

  def element(tag: String, attrs: Traversable[Attribute], nodes: Traversable[Node]): Element =
    Element(Try {
      def unwrap(e: Node): Dynamic = e match {
        case Element(el) => el.get
        case Text(text) => text.asInstanceOf[Dynamic]
      }
      val React = Dynamic.global.React
      val props = Dynamic.literal()
      for (attr <- attrs)
        attr.applyProps(props)
      val k = nodes map unwrap
      k.size match {
        case 0 => React.createElement(tag, props)
        case 1 => React.createElement(tag, props, k.head)
        case other => React.createElement.bind(null, tag, props).apply(null, k.toJSArray)
      }
    })

  def constant(element: Element): Element =
    Component[Unit](Try(Dynamic.global.React.createClass(Dynamic.literal(
      render = () => element.element.get,
      shouldComponentUpdate = () => false
    ))))(Unit)

  implicit def nodesToChildNodesOrAttributes(na: Traversable[NodeOrAttribute]): ChildNodesOrAttributes =
    ChildNodesOrAttributes(na)

  implicit def textsToChildNodesOrAttributes(na: Traversable[String]): ChildNodesOrAttributes =
    ChildNodesOrAttributes(na.map(Text))

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {

    def text(args: Any*): Text = Text(sc.s(args: _*))

  }

  private def attributes(all: Traversable[NodeOrAttribute]): Traversable[Attribute] =
    all flatMap {
      case attr: Attribute => List(attr)
      case ChildNodesOrAttributes(attrs) => attributes(attrs)
      case other => List()
    }

  private def nodes(all: Traversable[NodeOrAttribute]): Traversable[Node] =
    all flatMap {
      case node: Node => List(node)
      case ChildNodesOrAttributes(node) => nodes(node)
      case other => List()
    }

  implicit class StringOps(val value: String) extends AnyVal {

    def text: Text = Text(value)

    def apply(content: NodeOrAttribute*): Element =
      element(value, attributes(content), nodes(content))

    def apply(content: Traversable[NodeOrAttribute]): Element =
      element(value, attributes(content), nodes(content))

    def tag: Element = element(value, List(), List())

    def tag(content: NodeOrAttribute*): Element =
      element(value, attributes(content), nodes(content))

    def tag(content: Traversable[NodeOrAttribute]): Element =
      element(value, attributes(content), nodes(content))

  }

  implicit class SymbolOps(val value: Symbol) extends AnyVal {

    def apply(content: NodeOrAttribute*): Element =
      element(value.name, attributes(content), nodes(content))

    def apply(content: Traversable[NodeOrAttribute]): Element =
      element(value.name, attributes(content), nodes(content))

    def tag: Element = element(value.name, List(), List())

    def tag(content: NodeOrAttribute*): Element =
      element(value.name, attributes(content), nodes(content))

    def tag(content: Traversable[NodeOrAttribute]): Element =
      element(value.name, attributes(content), nodes(content))

  }

  implicit def stringToText(text: String): Text = Text(text)

  implicit class NativeElementOps(val element: Element) extends AnyVal {

    def constant: Element = Node.constant(element)

  }

  implicit class NativeElementFuncOps[Props](val f: Props => Element) extends AnyVal {

    def component: Component[Props] = Component.component(f)

  }

}
