/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2016-03-11
 */
package jclongra.locrefs;

public class LRobject {

  public Object value;

  public LRobject(Object value) {
    this.value = value;
  }

  public void set(Object value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }
}
