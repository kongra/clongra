/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2014-04-15
 */
package jclongra.core;

public final class Numbers {

  public static long uncheckedLongDivide(long x, long y) {
    return x / y;
  }

  public static long uncheckedLongMid(long low, long high) {
    return (low + high) >>> 1;
  }

  private Numbers() {
  }

}
