/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2014-04-01
 */
package jclongra.math;

public abstract class Infinity extends Number {

  @SuppressWarnings("synthetic-access")
  public static final Infinity POSITIVE = new Positive();

  @SuppressWarnings("synthetic-access")
  public static final Infinity NEGATIVE = new Negative();

  public static final class Positive extends Infinity {

    @SuppressWarnings("synthetic-access")
    private Positive() {
    }

    @Override
    public String toString() {
      return "+∞";
    }

    @Override
    public float floatValue() {
      return Float.POSITIVE_INFINITY;
    }

    @Override
    public double doubleValue() {
      return Double.POSITIVE_INFINITY;
    }

  }

  public static final class Negative extends Infinity {

    @SuppressWarnings("synthetic-access")
    private Negative() {
    }

    @Override
    public String toString() {
      return "-∞";
    }

    @Override
    public float floatValue() {
      return Float.NEGATIVE_INFINITY;
    }

    @Override
    public double doubleValue() {
      return Double.NEGATIVE_INFINITY;
    }
  }

  private Infinity() {
  }

  @Override
  public abstract String toString();

  @Override
  public int intValue() {
    throw new UnsupportedOperationException("No way to do this with " + this);
  }

  @Override
  public long longValue() {
    throw new UnsupportedOperationException("No way to do this with " + this);
  }

  public static Infinity parseDouble(double d) {
    if (Double.compare(d, Double.POSITIVE_INFINITY) == 0) {
      return POSITIVE;
    }
    if (Double.compare(d, Double.NEGATIVE_INFINITY) == 0) {
      return NEGATIVE;
    }

    throw new IllegalArgumentException(String.valueOf(d));
  }

  public static Infinity parseFloat(float f) {
    if (Float.compare(f, Float.POSITIVE_INFINITY) == 0) {
      return POSITIVE;
    }
    if (Float.compare(f, Float.NEGATIVE_INFINITY) == 0) {
      return NEGATIVE;
    }

    throw new IllegalArgumentException(String.valueOf(f));
  }
}
