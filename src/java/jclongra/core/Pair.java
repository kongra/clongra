/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-11-05
 */
package jclongra.core;

import clojure.lang.ArraySeq;
import clojure.lang.IObj;
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.Indexed;
import clojure.lang.Sequential;

public final class Pair<S, T> implements IObj, Indexed, IPersistentCollection,
    Sequential {

  private final S first;

  private final T second;

  private final IPersistentMap meta;

  /**
   * @param <X>
   * @param <Y>
   * @param first
   * @param second
   * @return
   */
  public static <X, Y> Pair<X, Y> of(X first, Y second) {
    return new Pair<X, Y>(first, second, null);
  }

  private Pair(S first, T second, IPersistentMap meta) {
    this.first = first;
    this.second = second;
    this.meta = meta;
  }

  public S first() {
    return first;
  }

  public T second() {
    return second;
  }

  @Override
  public String toString() {
    return seq().toString();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((first == null) ? 0 : first.hashCode());
    result = prime * result + ((second == null) ? 0 : second.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof Pair)) {
      return false;
    }
    Pair<?, ?> other = (Pair<?, ?>) obj;
    if (first == null) {
      if (other.first != null) {
        return false;
      }
    }
    else if (!first.equals(other.first)) {
      return false;
    }
    if (second == null) {
      if (other.second != null) {
        return false;
      }
    }
    else if (!second.equals(other.second)) {
      return false;
    }
    return true;
  }

  @Override
  public Object nth(int i) {
    if (i == 0) {
      return first;
    }
    else if (i == 1) {
      return second;
    }
    else {
      throw new IndexOutOfBoundsException();
    }
  }

  @Override
  public Object nth(int i, Object notFound) {
    if (i == 0) {
      return first;
    }
    else if (i == 1) {
      return second;
    }
    else {
      return notFound;
    }
  }

  @Override
  public int count() {
    return 2;
  }

  @Override
  public ISeq seq() {
    return ArraySeq.create(first, second);
  }

  @Override
  public IPersistentCollection cons(Object o) {
    return seq().cons(o);
  }

  @Override
  public IPersistentCollection empty() {
    return seq().empty();
  }

  @Override
  public boolean equiv(Object o) {
    return this.equals(o) || seq().equiv(o);
  }

  @Override
  public IPersistentMap meta() {
    return meta;
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new Pair<S, T>(first, second, meta);
  }

}
