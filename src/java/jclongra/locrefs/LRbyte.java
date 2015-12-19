/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-12-19
 */
package jclongra.locrefs;

public class LRbyte {

  public byte value;

  public LRbyte(byte value) {
    this.value = value;
  }

  public void set(byte value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

}
