/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-03-07
 */
package jclongra.core;

import java.io.Serializable;

import clojure.lang.IPersistentCollection;
import clojure.lang.ISeq;
import clojure.lang.PersistentList;

public class LinkedSeq<T> implements ISeq, Serializable {

  public Node<T> head, tail;

  public LinkedSeq() {
  }

  public LinkedSeq(Node<T> head, Node<T> tail) {
    this.head = head;
    this.tail = tail;
  }

  public void add(T obj) {
    Node<T> newNode = new Node<T>();
    newNode.value = obj;
    if (isEmpty()) {
      head = tail = newNode;
    }
    else {
      tail.next = newNode;
      tail = newNode;
    }
  }

  public <S> LinkedSeq<S> map(FN<T, S> f) {
    if (isEmpty()) {
      return null;
    }
    LinkedSeq<S> newSeq = new LinkedSeq<S>();
    Node<T> pos = this.head;
    do {
      newSeq.add(f.call(pos.value));
      pos = pos.next;
    }
    while (pos != null);
    return newSeq;
  }

  public boolean isEmpty() {
    return head == null;
  }

  @Override
  public Object first() {
    if (isEmpty()) {
      return null;
    }
    return head.value;
  }

  @Override
  public ISeq next() {
    if (isEmpty() || head.next == null) {
      return null;
    }
    return new LinkedSeq<T>(head.next, tail);
  }

  @Override
  public ISeq cons(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public ISeq more() {
    ISeq s = next();
    if (s == null) {
      return PersistentList.EMPTY;
    }
    return s;
  }

  @Override
  public int count() {
    Node<T> pos = this.head;
    int count = 0;
    while (pos != null) {
      count += 1;
      pos = pos.next;
    }
    return count;
  }

  @Override
  public IPersistentCollection empty() {
    return PersistentList.EMPTY;
  }

  @Override
  public boolean equiv(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public ISeq seq() {
    return isEmpty() ? null : this;
  }

  public static interface FN<D, C> {
    C call(D d);
  }

  public static class Node<E> implements Serializable {

    public E value;

    public Node<E> next;

    public Node(E value, Node<E> next) {
      this.value = value;
      this.next = next;
    }

    public Node() {

    }

  }
}
