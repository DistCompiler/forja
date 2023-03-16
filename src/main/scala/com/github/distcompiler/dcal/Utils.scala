package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.IR.Node

object Utils {
  object IRUtils {
    def stringifyNode(node: IR.Node): Iterator[Char] =
      node match {
        case Node.Name(name) => name.iterator

        case Node.Let(name, binding, body) =>
          s"LET $name == ".iterator ++
            stringifyNodes(binding) ++
            "\nIN ".iterator ++
            stringifyNodes(body) ++
            "\n".iterator

        case Node.Uninterpreted(text) => text.iterator

        case Node.MapOnSet(set, setMember, proc) =>
                    "{ ".iterator ++
                      stringifyNodes(proc) ++
                      ": ".iterator ++
                      s"$setMember \\in ".iterator ++
                      stringifyNodes(set) ++
                      " }".iterator

        case Node.FilterOnSet(set, setMember, pred) =>
          "{ ".iterator ++
            s"$setMember \\in ".iterator ++
            stringifyNodes(set) ++
            ": ".iterator ++
            stringifyNodes(pred) ++
            " }".iterator
      }

    /**
     * Converts a list of IR.Node representing TLA+ code to a String of TLA+ code
     */
    def stringifyNodes(nodes: List[IR.Node]): Iterator[Char] =
      nodes.iterator.flatMap(stringifyNode)

    def stringifyParams(params: List[String]): Iterator[Char] =
      params.mkString(", ").iterator

    def stringifyDefinition(definition: IR.Definition): Iterator[Char] =
      definition.name.iterator ++
        "(".iterator ++
        stringifyParams(definition.params) ++
        ") ==\n".iterator ++
        stringifyNodes(definition.body) ++
        "\n".iterator

    def stringifyModule(module: IR.Module): Iterator[Char] =
      s"---- MODULE ${module.name} ----".iterator ++
        "\n".iterator ++
        "EXTENDS Naturals\n" ++
        module.definitions.iterator.flatMap(stringifyDefinition) ++
        "====".iterator ++
        "\n".iterator
  }
}
