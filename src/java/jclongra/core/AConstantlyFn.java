/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2012-07-20
 */
package jclongra.core;

import clojure.lang.IFn;
import clojure.lang.ISeq;

public abstract class AConstantlyFn implements IFn {

  protected abstract Object constantly();

  @Override
  public final Object applyTo(ISeq arglist) {
    return constantly();
  }

  @Override
  public final Object invoke() {
    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15, Object arg16, Object arg17, Object arg18,
      Object arg19, Object arg20, Object... args) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15, Object arg16, Object arg17, Object arg18,
      Object arg19, Object arg20) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15, Object arg16, Object arg17, Object arg18,
      Object arg19) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15, Object arg16, Object arg17, Object arg18) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15, Object arg16, Object arg17) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15, Object arg16) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14, Object arg15) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13,
      Object arg14) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12, Object arg13) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11, Object arg12) {
    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10, Object arg11) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9, Object arg10) {
    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8,
      Object arg9) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7, Object arg8) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6, Object arg7) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5, Object arg6) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3,
      Object arg4, Object arg5) {

    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2, Object arg3) {
    return constantly();
  }

  @Override
  public final Object invoke(Object arg1, Object arg2) {
    return constantly();
  }

  @Override
  public final Object invoke(Object arg1) {
    return constantly();
  }

  @Override
  public final void run() {
    constantly();
  }

  @Override
  public final Object call() throws Exception {
    return constantly();
  }

}
