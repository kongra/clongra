/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-11-08
 */
package jclongra;

public class ValueError extends RuntimeException {

  public ValueError() {
  }

  public ValueError(String message) {
    super(message);
  }

  public ValueError(Throwable cause) {
    super(cause);
  }

  public ValueError(String message, Throwable cause) {
    super(message, cause);
  }

}
