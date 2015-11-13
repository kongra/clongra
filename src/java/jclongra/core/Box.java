/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-10-11
 */
package jclongra.core;

public final class Box<T> {

  public static <S> Box<S> of(S value) {
    return new Box<S>(value);
  }

  public T value;

  private Box(T value) {
    this.value = value;
  }

  public T get() {
    return value;
  }

  public void set(T value) {
    this.value = value;
  }

}
