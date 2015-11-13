/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-10-06
 */
package jclongra.core;

import java.util.HashMap;
import java.util.Map;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.RestFn;

public final class Memo extends RestFn {

  public Memo(IFn f, IFn pred, boolean traceHits) {
    this.f = f;
    this.pred = pred;
    this.traceHits = traceHits;

    if (!traceHits) {
      this.fHits = -1;
      this.cacheHits = -1;
    }
  }

  public final synchronized void resetAll(boolean clearHits) {
    this.cache.clear();
    if (traceHits && clearHits) {
      this.fHits = 0;
      this.cacheHits = 0;
    }
  }

  public final synchronized boolean reset(Object key) {
    return this.cache.remove(key) != null;
  }

  public final synchronized int cacheSize() {
    return this.cache.size();
  }

  public final synchronized long cacheHits() {
    return this.cacheHits;
  }

  public final synchronized long fHits() {
    return this.fHits;
  }

  public final synchronized long totalHits() {
    return this.cacheHits + this.fHits;
  }

  @Override
  protected final Object doInvoke(Object args) {
    synchronized (this) {
      if (cache.containsKey(args)) {
        if (traceHits) {
          cacheHits += 1;
        }
        return cache.get(args);
      }
    }

    Object value = f.applyTo((ISeq) args);
    Object predValue = pred.invoke(value);

    synchronized (this) {
      if (predValue != null && predValue != Boolean.FALSE) {
        cache.put(args, value);
      }
      if (traceHits) {
        fHits += 1;
      }
      return value;
    }
  }

  @Override
  public final int getRequiredArity() {
    return 0;
  }

  private final IFn f;

  private final IFn pred;

  private final boolean traceHits;

  private final Map<Object, Object> cache = new HashMap<Object, Object>();

  private long fHits;

  private long cacheHits;

}
