/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-11-13
 */
package jclongra.core.eseq;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import jclongra.core.Proxies;
import jclongra.math.Infinity;
import clojure.lang.IFn;
import clojure.lang.IPersistentCollection;
import clojure.lang.ISeq;
import clojure.lang.Numbers;
import clojure.lang.PersistentList;
import clojure.lang.RT;
import clojure.lang.Util;

public abstract class ESeq implements List, IEnhancedSeq {

  public static Object withLen(final IFn len, Object coll) {
    return new ESeq(origin(coll)) {
      @Override
      public Number len() {
        return (Number) len.invoke();
      }
    };
  }

  public static Object withEnth(final IFn enth, Object coll) {
    return new ESeq(origin(coll)) {
      @Override
      public Object enth(Number n) {
        return enth.invoke(n);
      }
    };
  }

  @Override
  public Number len() {
    if (origin instanceof IEnhancedSeq) {
      return ((IEnhancedSeq) origin).len();
    }
    return origin.count();
  }

  @Override
  public Object enth(Number n) {
    if (origin instanceof IEnhancedSeq) {
      return ((IEnhancedSeq) origin).enth(n);
    }
    return RT.nth(origin, RT.intCast(n));
  }

  @Override
  public final int count() {
    return RT.intCast(len());
  }

  @Override
  public final Object nth(int i) {
    return enth(i);
  }

  @Override
  public final Object nth(int i, Object notFound) {
    try {
      return enth(i);
    }
    catch (IndexOutOfBoundsException e) {
      return notFound;
    }
  }

  @Override
  public final IPersistentCollection cons(Object o) {
    return origin.cons(o);
  }

  @Override
  public final IPersistentCollection empty() {
    return origin.empty();
  }

  @Override
  public final ISeq seq() {
    return RT.seq(origin);
  }

  @Override
  public final int hasheq() {
    return Util.hasheq(origin);
  }

  @Override
  public final int hashCode() {
    return hasheq();
  }

  @Override
  public boolean equiv(Object o) {
    return Util.equiv(origin, o);
  }

  @Override
  public final boolean equals(Object obj) {
    return equiv(obj);
  }

  @Override
  public final String toString() {
    return (String) Proxies.tstr.fn().invoke(origin);
  }

  @Override
  public final boolean add(Object e) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final boolean remove(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final boolean addAll(Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final boolean addAll(int index, Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final boolean removeAll(Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final boolean retainAll(Collection c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final void clear() {
    throw new UnsupportedOperationException();
  }

  @Override
  public final Object set(int index, Object element) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final void add(int index, Object element) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final Object remove(int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final int size() {
    return count();
  }

  private static final Long ZERO = 0L;

  @Override
  public final boolean isEmpty() {
    Number n = len();
    if (Infinity.POSITIVE == n) {
      return false;
    }
    return Numbers.equiv(n, ZERO);
  }

  @Override
  public final boolean contains(Object o) {
    return lst().contains(o);
  }

  @Override
  public final Iterator iterator() {
    return lst().iterator();
  }

  @Override
  public final Object[] toArray() {
    return RT.toArray(origin);
  }

  @Override
  public final Object[] toArray(Object[] a) {
    throw new UnsupportedOperationException();
  }

  @SuppressWarnings("unchecked")
  @Override
  public final boolean containsAll(Collection c) {
    return lst().containsAll(c);
  }

  @Override
  public final Object get(int index) {
    return nth(index);
  }

  @Override
  public final int indexOf(Object o) {
    return lst().indexOf(o);
  }

  @Override
  public final int lastIndexOf(Object o) {
    return lst().lastIndexOf(o);
  }

  @Override
  public final ListIterator listIterator() {
    return lst().listIterator();
  }

  @Override
  public final ListIterator listIterator(int index) {
    return lst().listIterator(index);
  }

  @Override
  public final List subList(int fromIndex, int toIndex) {
    return lst().subList(fromIndex, toIndex);
  }

  private final IPersistentCollection origin;

  ESeq(IPersistentCollection origin) {
    this.origin = origin;
  }

  private static IPersistentCollection origin(Object coll) {
    if (coll == null) {
      return PersistentList.EMPTY;
    }
    if (coll instanceof IPersistentCollection) {
      return (IPersistentCollection) coll;
    }
    ISeq result = RT.seq(coll);
    return result != null ? result : PersistentList.EMPTY;
  }

  private List lst() {
    return (List) origin;
  }

}
