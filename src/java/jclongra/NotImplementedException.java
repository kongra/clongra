/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2006-12-04
 */
package jclongra;

/**
 * @author kongra
 * @version $Revision:$
 */
public class NotImplementedException extends RuntimeException {

  private static final long serialVersionUID = 3986154277782797323L;

  /**
     *
     */
  public NotImplementedException() {
    super();
  }

  /**
   * @param message
   */
  public NotImplementedException(String message) {
    super(message);
  }

  /**
   * @param cause
   */
  public NotImplementedException(Throwable cause) {
    super(cause);
  }

  /**
   * @param message
   * @param cause
   */
  public NotImplementedException(String message, Throwable cause) {
    super(message, cause);
  }

}
