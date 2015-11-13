/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-11-05
 */
package jclongra.core;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import clojure.lang.ISeq;
import clojure.lang.Ratio;

public final class Bits {

  public static boolean getBoolean(byte[] b, int off) {
    return b[off] != 0;
  }

  public static byte getByte(byte[] b, int off) {
    return b[off];
  }

  public static char getChar(byte[] b, int off) {
    return (char) (((b[off + 1] & 0xFF) << 0) + ((b[off + 0]) << 8));
  }

  public static short getShort(byte[] b, int off) {
    return (short) (((b[off + 1] & 0xFF) << 0) + ((b[off + 0]) << 8));
  }

  public static int getInt(byte[] b, int off) {
    return ((b[off + 3] & 0xFF) << 0) + ((b[off + 2] & 0xFF) << 8)
        + ((b[off + 1] & 0xFF) << 16) + ((b[off + 0]) << 24);
  }

  public static float getFloat(byte[] b, int off) {
    int i =
        ((b[off + 3] & 0xFF) << 0) + ((b[off + 2] & 0xFF) << 8)
            + ((b[off + 1] & 0xFF) << 16) + ((b[off + 0]) << 24);
    return Float.intBitsToFloat(i);
  }

  public static long getLong(byte[] b, int off) {
    return ((b[off + 7] & 0xFFL) << 0) + ((b[off + 6] & 0xFFL) << 8)
        + ((b[off + 5] & 0xFFL) << 16) + ((b[off + 4] & 0xFFL) << 24)
        + ((b[off + 3] & 0xFFL) << 32) + ((b[off + 2] & 0xFFL) << 40)
        + ((b[off + 1] & 0xFFL) << 48) + (((long) b[off + 0]) << 56);
  }

  public static long getUInt48(byte[] b, int off) {
    return ((b[off + 5] & 0xFFL) << 0) + ((b[off + 4] & 0xFFL) << 8)
        + ((b[off + 3] & 0xFFL) << 16) + ((b[off + 2] & 0xFFL) << 24)
        + ((b[off + 1] & 0xFFL) << 32) + ((b[off + 0] & 0xFFL) << 40);
  }

  public static double getDouble(byte[] b, int off) {
    long j =
        ((b[off + 7] & 0xFFL) << 0) + ((b[off + 6] & 0xFFL) << 8)
            + ((b[off + 5] & 0xFFL) << 16) + ((b[off + 4] & 0xFFL) << 24)
            + ((b[off + 3] & 0xFFL) << 32) + ((b[off + 2] & 0xFFL) << 40)
            + ((b[off + 1] & 0xFFL) << 48) + (((long) b[off + 0]) << 56);
    return Double.longBitsToDouble(j);
  }

  /**
   * Returns a bitset containing the values in bytes. The byte-ordering of bytes
   * must be big-endian which means the most significant bit is in element 0.
   *
   * <p>
   * Source: http://www.exampledepot.com/egs/java.util/Bits2Array.html
   *
   * @param bytes
   * @return
   */
  public static BitSet bytesToBitSet(byte[] bytes) {
    BitSet bits = new BitSet();
    for (int i = 0; i < bytes.length * 8; i++) {
      if ((bytes[bytes.length - i / 8 - 1] & (1 << (i % 8))) > 0) {
        bits.set(i);
      }
    }
    return bits;
  }

  public static void putBoolean(byte[] b, int off, boolean val) {
    b[off] = (byte) (val ? 1 : 0);
  }

  public static byte[] booleanToBytes(boolean b) {
    byte[] arr = new byte[1];
    putBoolean(arr, 0, b);
    return arr;
  }

  public static void putByte(byte[] b, int off, byte val) {
    b[off] = val;
  }

  public static byte[] byteToBytes(byte b) {
    byte[] arr = new byte[1];
    putByte(arr, 0, b);
    return arr;
  }

  public static void putChar(byte[] b, int off, char val) {
    b[off + 1] = (byte) (val >>> 0);
    b[off + 0] = (byte) (val >>> 8);
  }

  public static byte[] charToBytes(char c) {
    byte[] arr = new byte[2];
    putChar(arr, 0, c);
    return arr;
  }

  public static void putShort(byte[] b, int off, short val) {
    b[off + 1] = (byte) (val >>> 0);
    b[off + 0] = (byte) (val >>> 8);
  }

  public static byte[] shortToBytes(short s) {
    byte[] arr = new byte[2];
    putShort(arr, 0, s);
    return arr;
  }

  public static void putInt(byte[] b, int off, int val) {
    b[off + 3] = (byte) (val >>> 0);
    b[off + 2] = (byte) (val >>> 8);
    b[off + 1] = (byte) (val >>> 16);
    b[off + 0] = (byte) (val >>> 24);
  }

  public static byte[] intToBytes(int i) {
    byte[] arr = new byte[4];
    putInt(arr, 0, i);
    return arr;
  }

  public static void putFloat(byte[] b, int off, float val) {
    int i = Float.floatToIntBits(val);
    b[off + 3] = (byte) (i >>> 0);
    b[off + 2] = (byte) (i >>> 8);
    b[off + 1] = (byte) (i >>> 16);
    b[off + 0] = (byte) (i >>> 24);
  }

  public static byte[] floatToBytes(float f) {
    byte[] arr = new byte[4];
    putFloat(arr, 0, f);
    return arr;
  }

  public static void putLong(byte[] b, int off, long val) {
    b[off + 7] = (byte) (val >>> 0);
    b[off + 6] = (byte) (val >>> 8);
    b[off + 5] = (byte) (val >>> 16);
    b[off + 4] = (byte) (val >>> 24);
    b[off + 3] = (byte) (val >>> 32);
    b[off + 2] = (byte) (val >>> 40);
    b[off + 1] = (byte) (val >>> 48);
    b[off + 0] = (byte) (val >>> 56);
  }

  public static byte[] longToBytes(long l) {
    byte[] arr = new byte[8];
    putLong(arr, 0, l);
    return arr;
  }

  public static void putUInt48(byte[] b, int off, long val) {
    b[off + 5] = (byte) (val >>> 0);
    b[off + 4] = (byte) (val >>> 8);
    b[off + 3] = (byte) (val >>> 16);
    b[off + 2] = (byte) (val >>> 24);
    b[off + 1] = (byte) (val >>> 32);
    b[off + 0] = (byte) (val >>> 40);
  }

  public static byte[] UInt48ToBytes(long l) {
    byte[] arr = new byte[6];
    putUInt48(arr, 0, l);
    return arr;
  }

  public static void putDouble(byte[] b, int off, double val) {
    long j = Double.doubleToLongBits(val);
    b[off + 7] = (byte) (j >>> 0);
    b[off + 6] = (byte) (j >>> 8);
    b[off + 5] = (byte) (j >>> 16);
    b[off + 4] = (byte) (j >>> 24);
    b[off + 3] = (byte) (j >>> 32);
    b[off + 2] = (byte) (j >>> 40);
    b[off + 1] = (byte) (j >>> 48);
    b[off + 0] = (byte) (j >>> 56);
  }

  public static byte[] doubleToBytes(double d) {
    byte[] arr = new byte[8];
    putDouble(arr, 0, d);
    return arr;
  }

  /**
   * Returns a byte array of at least length 1. The most significant bit in the
   * result is guaranteed not to be a 1 (since BitSet does not support sign
   * extension). The byte-ordering of the result is big-endian which means the
   * most significant bit is in element 0. The bit at index 0 of the bit set is
   * assumed to be the least significant bit.
   *
   * <p>
   * Source: http://www.exampledepot.com/egs/java.util/Bits2Array.html
   *
   * @param bits
   * @return
   */
  public static byte[] bitSetToBytes(BitSet bits) {
    byte[] bytes = new byte[bits.length() / 8 + 1];
    for (int i = 0; i < bits.length(); i++) {
      if (bits.get(i)) {
        bytes[bytes.length - i / 8 - 1] |= 1 << (i % 8);
      }
    }
    return bytes;
  }

  private static final double LN2 = Math.log(2);

  /**
   * @param n
   *          either negative or non-negative
   * @return bit length for n
   */
  public static int bitLength(long n) {
    n = n < 0 ? -n : n + 1;
    double lg2 = Math.log(n) / LN2;
    return (int) Math.ceil(lg2);
  }

  private static final Long ZERO = 0L;

  private static final Long ONE = 1L;

  /**
   * @param n
   *          must be non-negative
   * @return a list of little-endian bits for the argument n
   */
  public static List<Long> longBits(long n) {
    if (n < 0) {
      throw new IllegalArgumentException("Negative argument " + n);
    }
    List<Long> result = new ArrayList<>(bitLength(n));
    if (n == 0) {
      result.add(ZERO);
      return result;
    }

    while (n != 0) {
      if (n % 2 == 0) {
        result.add(ZERO);
      }
      else {
        result.add(ONE);
      }
      n = n >> 1;
    }

    return result;
  }

  /**
   * Converts the given string to an array of bytes. Assumes the chars to be
   * non-ASCII (16 bit).
   *
   * @param str
   * @param offset
   *          if you want to skip some (offset) chars from the start of str
   * @return
   */
  public static byte[] stringToBytesUTF(String str, int offset) {
    int n = str.length() - offset;
    byte[] b = new byte[n << 1];
    for (int i = 0; i < n; i++) {
      char strChar = str.charAt(i + offset);
      int bpos = i << 1;
      b[bpos] = (byte) ((strChar & 0xFF00) >> 8);
      b[bpos + 1] = (byte) (strChar & 0x00FF);
    }
    return b;
  }

  /**
   * Converts the given array of bytes to a string. Assumes the chars to be
   * non-ASCII (16 bit).
   *
   * @param bytes
   * @return
   */
  public static String bytesToStringUTF(byte[] bytes) {
    char[] buffer = new char[bytes.length >> 1];
    for (int i = 0; i < buffer.length; i++) {
      int bpos = i << 1;
      char c =
          (char) (((bytes[bpos] & 0x00FF) << 8) + (bytes[bpos + 1] & 0x00FF));
      buffer[i] = c;
    }
    return new String(buffer);
  }

  public static long[] bytesToLongsArray(byte[] bytes) {
    if (bytes.length % 8 != 0) {
      throw new IllegalArgumentException(
          "The length of byte array must be a multiple of 8.");
    }
    long[] result = new long[bytes.length / 8];
    for (int i = 0; i < result.length; i++) {
      result[i] = getLong(bytes, i * 8);
    }
    return result;
  }

  public static List<Long> bytesToLongsList(byte[] bytes) {
    if (bytes.length % 8 != 0) {
      throw new IllegalArgumentException(
          "The length of byte array must be a multiple of 8.");
    }
    int n = bytes.length / 8;
    List<Long> result = new ArrayList<Long>(n);
    for (int i = 0; i < n; i++) {
      result.add(getLong(bytes, i * 8));
    }
    return result;
  }

  public static byte[] longsArrayToBytes(long[] longs) {
    byte[] result = new byte[longs.length * 8];
    for (int i = 0; i < longs.length; i++) {
      putLong(result, i * 8, longs[i]);
    }
    return result;
  }

  public static byte[] longsSeqToBytes(ISeq seq, int seqCount) {
    byte[] result = new byte[seqCount * 8];
    if (seqCount == 0) {
      return result;
    }
    int i = 0;
    ISeq s = seq;
    while (true) {
      if (null == s) {
        break;
      }
      long l = (Long) s.first();
      putLong(result, i * 8, l);
      s = s.next();
      i += 1;
    }
    return result;
  }

  public static byte[] bigDecimalToBytes(BigDecimal d) {
    byte[] unscaledBytes = d.unscaledValue().toByteArray();
    byte[] result = new byte[4 + unscaledBytes.length];
    putInt(result, 0, d.scale());
    System.arraycopy(unscaledBytes, 0, result, 4, unscaledBytes.length);

    return result;
  }

  public static BigDecimal bytesToBigDecimal(byte[] bytes) {
    int scale = getInt(bytes, 0);
    int n = bytes.length - 4;
    byte[] unscaledBytes = new byte[n];
    System.arraycopy(bytes, 4, unscaledBytes, 0, n);

    return new BigDecimal(new BigInteger(unscaledBytes), scale);
  }

  public static byte[] ratioToBytes(Ratio ratio) {
    byte[] numer = ratio.numerator.toByteArray();
    byte[] denom = ratio.denominator.toByteArray();

    byte[] result = new byte[4 + numer.length + denom.length];

    putInt(result, 0, numer.length);
    System.arraycopy(numer, 0, result, 4, numer.length);
    System.arraycopy(denom, 0, result, 4 + numer.length, denom.length);

    return result;
  }

  public static Ratio bytesToRatio(byte[] bytes) {
    int numerlen = getInt(bytes, 0);
    byte[] numer = new byte[numerlen];

    int denomlen = bytes.length - numerlen - 4;
    byte[] denom = new byte[denomlen];

    System.arraycopy(bytes, 4, numer, 0, numerlen);
    System.arraycopy(bytes, 4 + numerlen, denom, 0, denomlen);

    return new Ratio(new BigInteger(numer), new BigInteger(denom));
  }

  private Bits() {
    ;
  }
}
