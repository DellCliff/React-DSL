package dellcliff.html.react

import scala.scalajs.js.Dynamic


sealed case class Component[Props] private(component: Dynamic) {
  def apply(props: Props): Element = {
    Element(Dynamic.global.React.createElement(
      component,
      Dynamic.literal(data = props.asInstanceOf[Dynamic])))
  }
}

object Component {

  def component[Props](f: Props => Element): Component[Props] = {
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

