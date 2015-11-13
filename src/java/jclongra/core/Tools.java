/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2010-06-20
 */
package jclongra.core;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

import clojure.lang.IFn;

public final class Tools {

  public static boolean refEqual(Object obj1, Object obj2) {
    return obj1 == obj2;
  }

  public static Process execNowait(Object cmd, File dir, String[] envp)
      throws IOException {
    if (cmd instanceof String) {
      return Runtime.getRuntime().exec((String) cmd, envp, dir);
    }
    return Runtime.getRuntime().exec((String[]) cmd, envp, dir);
  }

  @SuppressWarnings("null")
  public static int exec(Object cmd, File dir, String[] envp, IFn outfn,
      IFn errfn) throws InterruptedException, IOException {

    Process process = execNowait(cmd, dir, envp);
    BufferedReader bri = null, bre = null;

    try {
      if (outfn != null) {
        bri =
            new BufferedReader(new InputStreamReader(process.getInputStream()));
      }
      if (errfn != null) {
        bre =
            new BufferedReader(new InputStreamReader(process.getErrorStream()));
      }

      String line;

      if (outfn != null) {
        while ((line = bri.readLine()) != null) {
          try {
            outfn.invoke(line);
          }
          catch (Exception e) {
            throw new RuntimeException(e);
          }
        }
      }
      if (errfn != null) {
        while ((line = bre.readLine()) != null) {
          try {
            errfn.invoke(line);
          }
          catch (Exception e) {
            throw new RuntimeException(e);
          }
        }
      }
    }
    finally {
      if (bri != null) {
        try {
          bri.close();
        }
        catch (IOException e) {
          e.printStackTrace(System.err);
        }
      }
      if (bre != null) {
        try {
          bre.close();
        }
        catch (IOException e) {
          e.printStackTrace(System.err);
        }
      }
    }

    return process.waitFor();
  }

  private Tools() {
    ;
  }
}
