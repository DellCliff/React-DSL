package dellcliff.html.react

import scala.scalajs.js.Dynamic
import scala.scalajs.js.JSConverters._

sealed trait Node extends NodeOrAttribute

sealed case class TextElement(text: String) extends Node

sealed case class NativeElement(element: Node.NativeDefs.ReactElement) extends Node

object Node {
  type Nodes = Traversable[Node]

  object NativeDefs {

    @scalajs.js.native
    sealed trait ReactElement extends scalajs.js.Any

    @scalajs.js.native
    sealed trait ReactComponentElement extends ReactElement

    @scalajs.js.native
    sealed trait ReactDOMElement extends ReactElement

    @scalajs.js.native
    sealed trait ReactComponent[Props] extends scalajs.js.Any

    @scalajs.js.annotation.ScalaJSDefined
    private[Node] sealed trait ReactProps[Props] extends scalajs.js.Object {
      var data: Props
    }

  }

  import NativeDefs._

  private def propsToLiteral(attrs: Attribute.Attributes): Dynamic = {
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

  object Unsafe {
    def nativeElement(tag: String, attrs: Attribute.Attributes, nodes: Nodes): NativeElement = {
      val React = Dynamic.global.React
      val props = propsToLiteral(attrs)
      def unwrap(e: NodeOrAttribute): Traversable[Any] = e match {
        case NativeElement(el) => List(el)
        case TextElement(text) => List(text)
        case ChildNodesOrAttributes(elements) => elements flatMap unwrap
        case other => List()
      }
      val k = nodes flatMap unwrap
      NativeElement((k.size match {
        case 0 => React.createElement(tag, props)
        case 1 => React.createElement(tag, props, k.head.asInstanceOf[Dynamic])
        case other => React.createElement.bind(null, tag, props).apply(null, k.toJSArray)
      }).asInstanceOf[ReactDOMElement])
    }

    def nativeElement[Props](component: ReactComponent[Props], props: Props): NativeElement =
      NativeElement(Dynamic.global.React.createElement(component, new ReactProps[Props] {
        override var data: Props = props
      }).asInstanceOf[ReactComponentElement])

    def constant(e: NativeElement): NativeElement =
      nativeElement[Unit](component((x: Unit) => e), Unit)

    def component[Props](f: Props => NativeElement): ReactComponent[Props] = {
      val render: Dynamic => ReactElement = self =>
        f(self.props.asInstanceOf[ReactProps[Props]].data).element

      val shouldComponentUpdate: (Dynamic, Dynamic, Dynamic) => Boolean =
        (self, nextProps, nextState) => self.props.data != nextProps.data

      Dynamic.global.React.createClass(scala.scalajs.js.Dynamic.literal(
        render = render: scalajs.js.ThisFunction,
        shouldComponentUpdate = shouldComponentUpdate: scalajs.js.ThisFunction
      )).asInstanceOf[ReactComponent[Props]]
    }

    implicit class SymbolOps(val tag: Symbol) extends AnyVal {
      def apply(content: NodeOrAttribute*): NativeElement = tag(content)

      def apply(content: Traversable[NodeOrAttribute]): NativeElement =
        nativeElement(tag.name,
          content collect { case attr: Attribute => attr },
          content collect { case node: Node => node })
    }

    implicit class ComponentOps[Props](val c: ReactComponent[Props]) extends AnyVal {
      def apply(p: Props): NativeElement = Unsafe.nativeElement(c, p)
    }

  }

  def text(value: String): TextElement = TextElement(value)

  implicit class StringOps(val value: String) extends AnyVal {
    def text: TextElement = Node.text(value)
  }

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {
    def text(args: Any*): TextElement = Node.text(sc.s(args: _*))
  }

  implicit class ElementListOps(val nodes: Traversable[NodeOrAttribute]) extends AnyVal {
    def content: ChildNodesOrAttributes = ChildNodesOrAttributes(nodes)
  }

}
