/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-12-19
 */
package jclongra.locrefs;

public class LRshort {

  public short value;

  public LRshort(short value) {
    this.value = value;
  }

  public void set(short value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

}
