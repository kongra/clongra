/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-07-20
 */
package jclongra.core;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import clojure.lang.RT;
import clojure.lang.Seqable;

public final class ReturnSeq extends Return implements Seqable {

  public ReturnSeq(IFn generator) {
    super(generator);
  }

  @Override
  public ISeq seq() {
    return RT.seq(constantly());
  }

}
