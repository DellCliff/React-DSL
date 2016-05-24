package dellcliff.html.react

import scala.scalajs.js.Dynamic
import scala.util.{Failure, Try, Success}


sealed case class Component[Props](component: Try[Dynamic]) {

  def apply(props: Props): Element = component match {
    case Success(c) =>
      Element(Try(Dynamic.global.React.createElement(
        c,
        Dynamic.literal(data = props.asInstanceOf[Dynamic]))))
    case ex: Failure[_] => Element(ex)
  }

}

object Component {

  def component[Props](f: Props => Element): Component[Props] =
    Component(Try(Dynamic.global.React.createClass(Dynamic.literal(
      render = { self: Dynamic =>
        f(self.props.data.asInstanceOf[Props]).element.get
      }: scalajs.js.ThisFunction,
      shouldComponentUpdate = { (self: Dynamic, nextProps: Dynamic, nextState: Dynamic) =>
        self.props.data != nextProps.data
      }: scalajs.js.ThisFunction
    ))))

}
