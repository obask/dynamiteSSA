package sexpr

import java.io.{StringReader, StringWriter}

import clojure.java.api.Clojure

object PPrint {

  def toText(source: String): String = {
    val pw = new StringWriter()
    Clojure.`var`("clojure.pprint", "pprint")
    val result = clojure.lang.Compiler.load(new StringReader(
      "(require 'clojure.pprint)" +
        "(binding [clojure.pprint/*print-right-margin* 100]" +
        "(with-out-str (clojure.pprint/pprint '" + source + ")))"
    ))
    result.asInstanceOf[String]
  }

  def dump(ss: String): Unit = {
    println(toText(ss))
  }

}
