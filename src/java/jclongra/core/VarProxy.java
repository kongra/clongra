/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2013-11-28
 */
package jclongra.core;

import java.util.concurrent.atomic.AtomicReference;

import clojure.lang.IFn;
import clojure.lang.RT;
import clojure.lang.Var;

public final class VarProxy extends ProxyFn {

  public static VarProxy create() {
    return new VarProxy();
  }

  public static VarProxy create(Var var) {
    return new VarProxy().set(var);
  }

  public static VarProxy create(String ns, String name) {
    return create(RT.var(ns, name));
  }

  public static VarProxy create(String ns, String name, Object init) {
    return create(RT.var(ns, name, init));
  }

  public VarProxy set(Var var) {
    this.var.set(var);
    return this;
  }

  public Var get() {
    return this.var.get();
  }

  @Override
  public IFn fn() {
    return get().fn();
  }

  private VarProxy() {
    ;
  }

  private final AtomicReference<Var> var = new AtomicReference<>();

}
