/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-11-13
 */
package jclongra.core;

public final class Unicode {

  /**
   * Finds all occurences of a UnicodeEscape sequence \ u HD HD HD HD in the
   * source s and converts them into chars in the resulting string. HD here is a
   * HexDigit := {0 .. 9 | a .. f | A .. F}.
   *
   * @param s
   * @return
   */
  public static String unescape(String s) {
    int n = s.length();
    StringBuilder result = new StringBuilder(n);

    char[] hex = new char[4];

    for (int i = 0; i < n; i++) {
      char c = s.charAt(i);
      if (i + 5 >= n) {
        result.append(c);
      }
      else {
        char bs = c, u = s.charAt(i + 1), hd1 = s.charAt(i + 2), hd2 =
            s.charAt(i + 3), hd3 = s.charAt(i + 4), hd4 = s.charAt(i + 5);

        boolean notPrecededByBs = (i == 0) || (s.charAt(i - 1) != '\\');

        if (notPrecededByBs && bs == '\\' && u == 'u' && isHexDigit(hd1)
            && isHexDigit(hd2) && isHexDigit(hd3) && isHexDigit(hd4)) {

          hex[0] = hd1;
          hex[1] = hd2;
          hex[2] = hd3;
          hex[3] = hd4;

          char newChar = (char) Integer.parseInt(new String(hex), 16);
          result.append(newChar);

          i += 5;
        }
        else {
          result.append(c);
        }
      }
    }

    return result.toString();
  }

  public static boolean isHexDigit(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F');
  }

  private Unicode() {
    ;
  }

}
