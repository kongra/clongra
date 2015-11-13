/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Re-worked 2015-11-13
 */
package jclongra.math;

import java.math.BigInteger;

public final class BigIntegerUtils {

  static final int SIGNIFICAND_BITS = 52;

  static final int EXPONENT_BIAS = 1023;

  static final long SIGNIFICAND_MASK = 0x000fffffffffffffL;

  static final long SIGN_MASK = 0x8000000000000000L;

  /**
   * @see http://bugs.sun.com/view_bug.do?bug_id=7131192
   *
   * @param x
   * @return
   */
  public static double toDouble(BigInteger x) {
    BigInteger absX = x.abs();
    int exponent = absX.bitLength() - 1;
    // exponent == floor(log2(abs(x)))
    if (exponent < Long.SIZE - 1) {
      return x.longValue();
    }
    else if (exponent > Double.MAX_EXPONENT) {
      return x.signum() * Double.POSITIVE_INFINITY;
    }

    /*
     * We need the top SIGNIFICAND_BITS + 1 bits, including the "implicit" one
     * bit. To make rounding easier, we pick out the top SIGNIFICAND_BITS + 2
     * bits, so we have one to help us round up or down. twiceSignifFloor will
     * contain the top SIGNIFICAND_BITS + 2 bits, and signifFloor the top
     * SIGNIFICAND_BITS + 1.
     *
     * It helps to consider the real number signif = absX * 2^(SIGNIFICAND_BITS
     * - exponent).
     */
    int shift = exponent - SIGNIFICAND_BITS - 1;
    long twiceSignifFloor = absX.shiftRight(shift).longValue();
    long signifFloor = twiceSignifFloor >> 1;
    signifFloor &= SIGNIFICAND_MASK; // remove the implied bit

    /*
     * We round up if either the fractional part of signif is strictly greater
     * than 0.5 (which is true if the 0.5 bit is set and any lower bit is set),
     * or if the fractional part of signif is >= 0.5 and signifFloor is odd
     * (which is true if both the 0.5 bit and the 1 bit are set). This is
     * equivalent to the desired HALF_EVEN rounding behavior.
     */
    boolean increment =
        (twiceSignifFloor & 1) != 0
            && ((signifFloor & 1) != 0 || absX.getLowestSetBit() < shift);
    long signifRounded = increment ? signifFloor + 1 : signifFloor;
    long bits = (long) ((exponent + EXPONENT_BIAS)) << SIGNIFICAND_BITS;
    bits += signifRounded;
    /*
     * If signifRounded == 2^53, we'd need to set all of the significand bits to
     * zero and add 1 to the exponent. This is exactly the behavior we get from
     * just adding signifRounded to bits directly. If the exponent is
     * MAX_DOUBLE_EXPONENT, we round up (correctly) to Double.POSITIVE_INFINITY.
     */
    bits |= x.signum() & SIGN_MASK;
    return Double.longBitsToDouble(bits);
  }

}
