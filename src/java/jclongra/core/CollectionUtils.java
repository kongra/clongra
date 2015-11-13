/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2005-02-15
 */
package jclongra.core;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Grupuje funkcjonalność związaną z tworzeniem obiektów niektórych klas, m. in.
 * klas kolekcji.
 *
 * @author kongra
 * @version $Revision:$
 */
public final class CollectionUtils {

  /**
   * Zwraca nową listę będącą sumą czyli konkatenacją (w sensie
   * teoriomnogościowym nie algebraicznym) podanych kolekcji. Flaga pozwala
   * narzucić (lub nie) unikalność elementów w zwróconej liście.
   *
   * @param <T>
   * @param <S>
   * @param collections
   * @param keepUnique
   * @return
   */
  public static <T, S extends T> List<T> concat(Collection<S>[] collections,
      boolean keepUnique) {
    ArrayList<T> list = new ArrayList<T>();
    if (collections != null && collections.length != 0) {
      if (keepUnique) {
        for (Collection<S> collection : collections) {
          if (!CollectionUtils.isEmpty(collection)) {
            for (S element : collection) {
              if (!list.contains(element)) {
                list.add(element);
              }
            }
          }
        }
      }
      else {
        for (Collection<S> collection : collections) {
          if (!CollectionUtils.isEmpty(collection)) {
            for (S element : collection) {
              list.add(element);
            }
          }
        }
      }
    }
    return list;
  }

  /**
   * @param <T>
   * @param <S>
   * @param list
   * @param element
   */
  public static <T, S extends T> void addUnique(List<T> list, S element) {
    if (!list.contains(element)) {
      list.add(element);
    }
  }

  /**
   * @param <T>
   * @param <S>
   * @param list
   * @param toAdd
   */
  public static <T, S extends T> void addToFront(List<T> list, List<S> toAdd) {
    if (!CollectionUtils.isEmpty(toAdd)) {
      int i = 0;
      for (S element : toAdd) {
        list.add(i, element);
        i += 1;
      }
    }
  }

  /**
   * @param <T>
   * @param <S>
   * @param elements
   * @param element
   * @return
   */
  public static <T, S extends T> boolean contains(T[] elements, S element) {
    if (elements == null || elements.length == 0) {
      return false;
    }
    for (T t : elements) {
      if (t == element || t.equals(element)) {
        return true;
      }
    }
    return false;
  }

  /**
   * @param <K>
   * @param <V>
   * @return
   */
  public static <K, V> Map<K, V> createSortedMap() {
    return new TreeMap<K, V>();
  }

  /**
   * @param <K>
   * @param <V>
   * @param comparator
   * @return
   */
  public static <K, V> Map<K, V> createSortedMap(Comparator<K> comparator) {
    return new TreeMap<K, V>(comparator);
  }

  /**
   * Zwraca wartośc klucza dla podanej wartości w mapie.
   *
   * @param <K>
   * @param <V>
   * @param map
   * @param value
   * @return
   */
  public static <K, V> K getKeyForValue(Map<K, V> map, V value) {
    for (Entry<K, V> entry : map.entrySet()) {
      if (entry.getValue() == value || entry.getValue().equals(value)) {
        return entry.getKey();
      }
    }
    return null;
  }

  /**
   * @return
   */
  public static <T> Set<T> createSortedSet() {
    return new TreeSet<T>();
  }

  /**
   * @param col
   * @return
   */
  public static boolean isEmpty(Collection col) {
    return col == null || col.isEmpty();
  }

  /**
   * @param iterable
   * @return
   */
  public static boolean isEmpty(Iterable iterable) {
    return iterable == null || !iterable.iterator().hasNext();
  }

  /**
   * @param array
   * @return
   */
  public static boolean isEmpty(Object[] array) {
    return array == null || array.length == 0;
  }

  /**
   * @param map
   * @return
   */
  public static boolean isEmpty(Map map) {
    return map == null || map.isEmpty();
  }

  /**
   * @param <T>
   * @param object
   * @param array
   * @return
   */
  public static <T> boolean in(T object,
      @SuppressWarnings("unchecked") T... array) {
    if (array == null || array.length == 0) {
      return false;
    }
    else if (object == null) {
      for (int i = 0; i < array.length; i++) {
        if (array[i] == null) {
          return true;
        }
      }
      return false;
    }
    else {
      for (int i = 0; i < array.length; i++) {
        if (array[i].equals(object)) {
          return true;
        }
      }
      return false;
    }
  }

  /**
   * Zwraca ostatni element listy, o ile nie jest ona pusta lub null.
   *
   * @param <T>
   * @param elements
   * @return
   */
  public static <T> T last(List<T> elements) {
    if (isEmpty(elements)) {
      return null;
    }
    return elements.get(elements.size() - 1);
  }

  /**
   * @param <T>
   * @param elements
   * @return
   */
  public static <T> T first(List<T> elements) {
    if (isEmpty(elements)) {
      return null;
    }
    return elements.get(0);
  }

  /**
   * Bezpieczna metoda konwertująca dowolną kolekcję na tablicę. Kolekcja może
   * być null lub pusta. W takiej sytuacji zwrócona zostanie tablica o gługości
   * 0.
   *
   * @param <T>
   * @param elements
   * @param elementsType
   * @return
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] toArray(Collection<T> elements,
      Class<? super T> elementsType) {
    if (isEmpty(elements)) {
      return (T[]) Array.newInstance(elementsType, 0);
    }
    T[] array = (T[]) Array.newInstance(elementsType, elements.size());
    return elements.toArray(array);
  }

  /**
   * @param <T>
   * @param elements
   * @param elementsType
   * @return
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] copyOf(T[] elements, Class<? super T> elementsType) {
    if (isEmpty(elements)) {
      return (T[]) Array.newInstance(elementsType, 0);
    }
    T[] array = (T[]) Array.newInstance(elementsType, elements.length);
    System.arraycopy(elements, 0, array, 0, elements.length);
    return array;
  }

  /**
   * @param <T>
   * @param enumeration
   * @return
   */
  public static <T> Iterator<T> iterator(final Enumeration<T> enumeration) {
    return new Iterator<T>() {
      @Override
      public boolean hasNext() {
        return enumeration.hasMoreElements();
      }

      @Override
      public T next() {
        return enumeration.nextElement();
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  private CollectionUtils() {
    ;
  }

}
