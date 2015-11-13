/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-11-08
 */
package jclongra;

public class TypeError extends RuntimeException {

  public TypeError() {
  }

  public TypeError(String message, Throwable cause) {
    super(message, cause);
  }

  public TypeError(String message) {
    super(message);
  }

  public TypeError(Throwable cause) {
    super(cause);
  }

}
