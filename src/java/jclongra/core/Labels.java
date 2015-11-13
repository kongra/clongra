/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-08-30
 */
package jclongra.core;

import java.util.HashMap;
import java.util.Map;

import clojure.lang.RT;
import clojure.lang.Symbol;

public final class Labels {

  public static synchronized Symbol intern(String name) {
    Symbol s = symbols.get(name);
    if (s == null) {
      // The foreign invocation below is safe. We know that gensym does not
      // refer to Labels.
      s = (Symbol) RT.var("clojure.core", "gensym").invoke("L__");
      symbols.put(name, s);
    }
    return s;
  }

  public static synchronized void clearInterns() {
    symbols.clear();
  }

  private static final Map<String, Symbol> symbols =
      new HashMap<String, Symbol>();

  private Labels() {
    ;
  }

}
