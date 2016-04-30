package dellcliff.html.react

trait NodeOrAttribute

sealed case class ChildNodesOrAttributes
(nodesOrAttributes: Traversable[NodeOrAttribute]) extends NodeOrAttribute
