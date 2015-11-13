/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-11-08
 */
package jclongra.core;

import clojure.lang.AFunction;
import clojure.lang.IFn;
import clojure.lang.IObj;
import clojure.lang.IPersistentMap;

public final class Synchronized extends AFunction {

  public static Object invoke(Object monitor, IFn body) {
    synchronized (monitor) {
      return body.invoke();
    }
  }

  private final Object monitor;

  private final IFn body;

  private final IPersistentMap meta;

  public Synchronized(Object monitor, IFn body) {
    this(monitor, body, null);
  }

  public Synchronized(Object monitor, IFn body, IPersistentMap meta)
      throws IllegalArgumentException {
    super();
    if (monitor == null) {
      throw new IllegalArgumentException("The monitor must not be null.");
    }
    if (body == null) {
      throw new IllegalArgumentException("The body must not be null.");
    }
    this.monitor = monitor;
    this.body = body;
    this.meta = meta;
  }

  @Override
  public Object invoke() {
    return invoke(monitor, body);
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new Synchronized(this.monitor, this.body, meta);
  }

}
