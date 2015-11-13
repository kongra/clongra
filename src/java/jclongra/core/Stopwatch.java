/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-09-21
 */
package jclongra.core;

public final class Stopwatch {

  public static Stopwatch start() {
    return new Stopwatch();
  }

  public double elapsedTime() {
    return (System.nanoTime() - startTime) / 1e6d;
  }

  private final long startTime;

  private Stopwatch() {
    this.startTime = System.nanoTime();
  }

}
