/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-12-19
 */
package jclongra.locrefs;

public class LRdouble {

  public double value;

  public LRdouble(double value) {
    this.value = value;
  }

  public void set(double value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

}
