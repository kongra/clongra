/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-12-19
 */
package jclongra.locrefs;

public class LRlong {

  public long value;

  public LRlong(long value) {
    this.value = value;
  }

  public void set(long value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

}
