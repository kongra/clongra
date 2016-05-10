/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2016-05-09
 */
package jclongra.io;

import clojure.lang.IFn;

import java.io.IOException;
import java.io.Writer;

public class FnWriter extends Writer {

  private final IFn f;

  public FnWriter(IFn f) {
    this.f = f;
  }

  @Override
  public void write(char[] cbuf, int off, int len) throws IOException {
    f.invoke(String.valueOf(cbuf, off, len));
  }

  @Override
  public void flush() throws IOException {

  }

  @Override
  public void close() throws IOException {
  }

}
