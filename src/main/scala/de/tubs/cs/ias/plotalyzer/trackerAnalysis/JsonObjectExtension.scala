package de.tubs.cs.ias.plotalyzer.trackerAnalysis

import spray.json.{
  JsArray,
  JsBoolean,
  JsNull,
  JsNumber,
  JsObject,
  JsString,
  JsValue
}

import scala.reflect.ClassTag

object JsonObjectExtension {

  implicit class ExtendedJsObject(obj: JsObject) {

    def getOptionalField[X <: JsValue](field: String)(
        implicit tag: ClassTag[X]): Option[X] = {
      obj.fields.get(field) match {
        case Some(value: X) => Some(value)
        case Some(JsNull)   => None
        case Some(x) =>
          throw UnexpectedJsObjectField(x,
                                        obj,
                                        s"expected type ${tag.toString()}")
        case _ => None
      }
    }

    def getField[X <: JsValue](field: String)(implicit tag: ClassTag[X]): X = {
      getOptionalField[X](field) match {
        case Some(value: X) => value
        case None           => throw MissingJsObjectField(field, obj)
        case _ =>
          throw MissingJsObjectField(s"field $field is not of expected type",
                                     obj)
      }
    }

    def getOptionalField[X <: JsValue](fields: String*)(
        implicit tag: ClassTag[X]): Option[X] = {

      if (fields.isEmpty) {
        None
      } else {
        getOptionalField[X](fields.head) match {
          case Some(value: X) => Some(value)
          case None           => getOptionalField(fields.tail: _*)
          case Some(JsNull)   => None
          case _ =>
            throw MissingJsObjectField(s"field $fields is not of expected type",
                                       obj)
        }
      }
    }

    def getField[X <: JsValue](field: String*)(implicit tag: ClassTag[X]): X = {
      getOptionalField(field: _*) match {
        case Some(value: X) => value
        case None           => throw MissingJsObjectField(field.mkString(","), obj)
        case _ =>
          throw MissingJsObjectField(s"field $field is not of expected type",
                                     obj)
      }
    }

    def withOptionalFieldValue[T](field: String)(
        func: String => T): Option[T] = {
      object JsNullEncountered extends Throwable
      try {
        getOptionalField[JsValue](field) match {
          case Some(value) =>
            value match {
              case x: JsObject => throw UnexpectedJsObjectField(x, obj)
              case x: JsArray  => throw UnexpectedJsObjectField(x, obj)
              case JsString(value) =>
                if (value.nonEmpty) Some(func(value)) else None
              case JsNumber(value)    => Some(func(value.toString()))
              case boolean: JsBoolean => Some(func(boolean.value.toString))
              case JsNull =>
                throw JsNullEncountered
            }
          case None => None
        }
      } catch {
        case JsNullEncountered => None
      }
    }

    def withOptionalFieldValue[T](field: String*)(
        func: String => T): Option[T] = {
      if (field.nonEmpty) {
        withOptionalFieldValue[T](field.head)(func) match {
          case Some(value) => Some(value)
          case None        => withOptionalFieldValue(field.tail: _*)(func)
        }
      } else {
        None
      }
    }

    def withFieldValue(field: String)(func: String => PII): PII = {
      withOptionalFieldValue(field)(func)
        .getOrElse(MissingExpectedValue(field, obj))
    }

    def withFieldValue(field: String*)(func: String => PII): PII = {
      withOptionalFieldValue(field: _*)(func)
        .getOrElse(MissingExpectedValue(field.mkString(","), obj))
    }

    def withFieldValueList(field: String)(
        func: String => List[PII]): List[PII] = {
      withOptionalFieldValue(field)(func)
        .getOrElse(List(MissingExpectedValue(field, obj)))
    }

    def withFieldValueList(field: String*)(
        func: String => List[PII]): List[PII] = {
      withOptionalFieldValue(field: _*)(func)
        .getOrElse(List(MissingExpectedValue(field.mkString(","), obj)))
    }

    def withOptionalField[X <: JsValue](field: String)(func: X => PII)(
        implicit tag: ClassTag[X]): Option[PII] = {
      getOptionalField[X](field) match {
        case Some(value) => Some(func(value))
        case None        => None
      }
    }

    def withField[X <: JsValue](field: String)(func: X => PII)(
        implicit tag: ClassTag[X]): PII = {
      withOptionalField[X](field)(func)
        .getOrElse(MissingExpectedValue(field, obj))
    }

    def withOptionalFieldList[X <: JsValue](field: String)(
        func: X => List[PII])(implicit tag: ClassTag[X]): Option[List[PII]] = {
      getOptionalField[X](field) match {
        case Some(value) => Some(func(value))
        case None        => None
      }
    }

    def withFieldList[X <: JsValue](field: String)(func: X => List[PII])(
        implicit tag: ClassTag[X]): List[PII] = {
      withOptionalFieldList[X](field)(func) match {
        case Some(value) => value
        case None        => List(MissingExpectedValue(field, obj).asInstanceOf[PII])
      }
    }

  }

}
