/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-12-19
 */
package jclongra.locrefs;

public final class LRboolean {

  public boolean value;

  public LRboolean(boolean value) {
    this.value = value;
  }

  public void set(boolean value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

}
