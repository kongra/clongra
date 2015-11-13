/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2013-07-19
 */
package jclongra.core;

import java.util.Arrays;

public final class Bytes {

  public static byte[] arrayOfSize(int size) {
    return new byte[size];
  }

  public static Bytes ofSize(int size) {
    return valueOf(arrayOfSize(size));
  }

  public static Bytes valueOf(byte[] bytes) {
    return new Bytes(bytes);
  }

  private final byte[] bytes;

  private Bytes(byte[] bytes) {
    this.bytes = bytes;
  }

  public byte[] getBytes() {
    return this.bytes;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + Arrays.hashCode(bytes);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    Bytes other = (Bytes) obj;
    if (!Arrays.equals(bytes, other.bytes)) {
      return false;
    }
    return true;
  }

}
