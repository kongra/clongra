/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2016-03-20
 */
package jclongra.core;

import clojure.lang.RT;

public final class Primitives {

  public static boolean booleanNot(boolean b) {
    return !b;
  }

  public static long[] makeLongs(long size) {
    return new long[RT.intCast(size)];
  }

  public static double[] makeDoubles(long size) {
    return new double[RT.intCast(size)];
  }

}
