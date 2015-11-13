/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-07-19
 */
package jclongra.core;

import clojure.lang.IFn;

public class Return extends AConstantlyFn {

  public Return(IFn generator) {
    this.generator = generator;
  }

  private final IFn generator;

  @Override
  protected Object constantly() {
    return generator.invoke();
  }

}
