/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Re-worked 2015-11-13
 */
package jclongra.core.eseq;

import clojure.lang.IHashEq;
import clojure.lang.IPersistentCollection;
import clojure.lang.Indexed;
import clojure.lang.Sequential;

public interface IEnhancedSeq extends IPersistentCollection, Sequential,
    Indexed, IHashEq {

  Number len();

  Object enth(Number n);

}
