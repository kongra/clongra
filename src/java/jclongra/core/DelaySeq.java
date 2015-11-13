/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-07-20
 */
package jclongra.core;

import clojure.lang.Delay;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.RT;
import clojure.lang.Seqable;

public final class DelaySeq extends Delay implements Seqable {

  public DelaySeq(IFn fn) {
    super(fn);
  }

  @Override
  public ISeq seq() {
    return RT.seq(deref());
  }

}
