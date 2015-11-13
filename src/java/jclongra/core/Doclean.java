/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-11-08
 */
package jclongra.core;

import java.io.Closeable;

import jclongra.StateError;

public final class Doclean implements Closeable {

  public synchronized void register(Closeable c) {
    if (closed) {
      throw new StateError("The Doclean is closed, can't register " + c);
    }
    head = new Node(c, head);
  }

  @Override
  public void close() {
    synchronized (this) {
      if (closed) {
        return;
      }
      closed = true;
    }

    // NOBODY GETS HERE AFTER closed = true
    Node node = head;
    while (node != null) {
      try {
        node.closeable.close();
      }
      catch (Throwable t) {
        t.printStackTrace(System.err);
      }
      node = node.next;
    }

    head = null;
  }

  private boolean closed;

  private Node head;

  private static class Node {

    final Closeable closeable;

    final Node next;

    Node(Closeable closeable, Node next) {
      this.closeable = closeable;
      this.next = next;
    }

  }

}
