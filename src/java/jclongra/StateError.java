/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-11-08
 */
package jclongra;

public class StateError extends RuntimeException {

  public StateError() {
  }

  public StateError(String message) {
    super(message);
  }

  public StateError(Throwable cause) {
    super(cause);
  }

  public StateError(String message, Throwable cause) {
    super(message, cause);
  }

}
