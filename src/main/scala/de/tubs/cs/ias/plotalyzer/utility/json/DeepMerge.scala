package de.tubs.cs.ias.plotalyzer.utility.json

import spray.json.{JsArray, JsNull, JsObject, JsValue}

import scala.collection.mutable.{Map => MMap}

object DeepMerge {

  def arrayReduce(obj: JsObject): JsObject = {
    JsObject(
      obj.fields.map {
        case (key, value: JsArray) =>
          if (value.elements.distinct.length == 1) key -> value.elements.head
          else key -> JsArray(value.elements.distinct)
        case (key, value) => key -> value
      }
    )
  }

  def merge(elems: JsObject*): JsObject = {
    elems.reduce((lhs, rhs) => actualMerge(lhs, rhs))
  }

  private def actualMerge(rhs: JsObject,
                          lhs: JsObject,
                          depth: Int = 10): JsObject = {

    object MergeTooDeep extends Throwable

    def helper(rhs: JsObject, lhs: JsObject, depth: Int): JsObject = {
      if (depth == 0) throw MergeTooDeep
      val newMap: MMap[String, JsValue] = MMap()
      val keys = rhs.fields.keys ++ lhs.fields.keys
      keys.foreach { key =>
        val lhsVal = lhs.fields.get(key)
        val rhsVal = rhs.fields.get(key)
        (lhsVal, rhsVal) match {
          case (Some(lhsv: JsObject), Some(rhsv: JsObject)) =>
            newMap.addOne(key -> helper(lhsv, rhsv, depth - 1))
          case (Some(lhsv: JsArray), Some(rhsv: JsArray)) =>
            newMap.addOne(
              key -> JsArray(
                (lhsv.elements.toList ++ rhsv.elements.toList).toVector))
          case (Some(JsNull), Some(rhsv)) =>
            newMap.addOne(key -> rhsv)
          case (Some(lhsv), Some(JsNull)) =>
            newMap.addOne(key -> lhsv)
          case (Some(lhsv), Some(_)) =>
            newMap.addOne(key -> lhsv)
          case (None, Some(rhsv)) =>
            newMap.addOne(key -> rhsv)
          case (Some(lhsv), None) =>
            newMap.addOne(key -> lhsv)
          case (_, _) => // this cannot happen
        }
      }
      JsObject(newMap.toMap)
    }
    try {
      helper(rhs, lhs, depth)
    } catch {
      case MergeTooDeep =>
        println("FUCKED UP")
        println(rhs.prettyPrint)
        println(lhs.prettyPrint)
        JsObject()
    }
  }

}
