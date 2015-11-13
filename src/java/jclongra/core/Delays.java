/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-07-20
 */
package jclongra.core;

import clojure.lang.ISeq;
import clojure.lang.Seqable;

public final class Delays extends AConstantlyFn implements Seqable {

  public final Object items;

  public Delays(Object items) {
    this.items = items;
  }

  @Override
  protected ISeq constantly() {
    return (ISeq) Proxies.tseq.invoke(this);
  }

  @Override
  public ISeq seq() {
    return constantly();
  }

}
