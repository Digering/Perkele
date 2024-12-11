### Коллекции из пакета `java.util.concurrent`

В пакете `java.util.concurrent` предоставлены коллекции, которые предназначены для работы с многозадачностью и многопоточностью. Эти коллекции отличаются от стандартных коллекций из пакета `java.util` тем, что они обеспечивают безопасный доступ из нескольких потоков и предлагают дополнительные возможности, связанные с параллельной обработкой данных.

В данном пакете можно найти различные типы коллекций, которые поддерживают синхронизацию и позволяют эффективно работать с потоками. Рассмотрим основные из них.

### 1. **ConcurrentHashMap**

`ConcurrentHashMap` — это потокобезопасная версия `HashMap`, которая обеспечивает конкурентный доступ и изменяемость данных. Она позволяет нескольким потокам безопасно читать и модифицировать карту одновременно, без необходимости блокировать весь объект.

- **Основные особенности**:
  - **Разделение блокировок**: Для каждого сегмента карты используется отдельная блокировка, что увеличивает производительность при параллельной работе.
  - **Поддержка `null`**: В отличие от `HashMap`, `ConcurrentHashMap` не поддерживает `null` как ключи и значения.
  - **Параллельные операции**: Позволяет выполнять операции над картой, такие как `putIfAbsent()`, `remove()`, и `replace()` без блокировок.

Пример использования `ConcurrentHashMap`:

```java
import java.util.concurrent.*;

public class ConcurrentHashMapExample {
    public static void main(String[] args) {
        ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>();

        map.put("one", 1);
        map.put("two", 2);

        // Параллельный доступ
        new Thread(() -> {
            map.putIfAbsent("three", 3);
            System.out.println("Thread 1: " + map);
        }).start();

        new Thread(() -> {
            map.putIfAbsent("four", 4);
            System.out.println("Thread 2: " + map);
        }).start();
    }
}
```

### 2. **CopyOnWriteArrayList**

`CopyOnWriteArrayList` — это потокобезопасная версия `ArrayList`, которая создает копию всего списка каждый раз, когда в него вносятся изменения (например, при добавлении или удалении элементов). Это делает ее подходящей для ситуаций, когда список часто читается и редко изменяется.

- **Основные особенности**:
  - **Параллельные чтения**: Чтение из списка может происходить безопасно одновременно несколькими потоками.
  - **Модификация**: Каждая операция изменения (например, `add()`, `remove()`) приводит к созданию новой копии списка, что делает эту коллекцию менее эффективной для частых модификаций, но идеальной для редких изменений.

Пример использования `CopyOnWriteArrayList`:

```java
import java.util.concurrent.*;

public class CopyOnWriteArrayListExample {
    public static void main(String[] args) {
        CopyOnWriteArrayList<String> list = new CopyOnWriteArrayList<>();

        list.add("A");
        list.add("B");

        // Параллельные потоки могут безопасно читать список
        new Thread(() -> {
            list.add("C");
            System.out.println("Thread 1: " + list);
        }).start();

        new Thread(() -> {
            list.add("D");
            System.out.println("Thread 2: " + list);
        }).start();
    }
}
```

### 3. **BlockingQueue**

`BlockingQueue` — это интерфейс для коллекций, которые могут блокировать операции добавления или извлечения элементов, если коллекция пуста или полна. Это полезно при организации механизма очереди, когда один поток может добавлять элементы, а другой — извлекать их, и при этом не нужно беспокоиться о синхронизации.

Реализации `BlockingQueue`:

- **ArrayBlockingQueue**: Ограниченная очередь с фиксированным размером, элементы добавляются и извлекаются из нее в порядке FIFO.
- **LinkedBlockingQueue**: Очередь с динамическим размером (неограниченная или с установленным размером), может быть использована для более гибких решений.
- **PriorityBlockingQueue**: Очередь с приоритетами, элементы извлекаются в порядке их приоритетов.

Пример использования `BlockingQueue`:

```java
import java.util.concurrent.*;

public class BlockingQueueExample {
    public static void main(String[] args) throws InterruptedException {
        BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(10);

        // Параллельное добавление элементов в очередь
        new Thread(() -> {
            try {
                queue.put(1);
                queue.put(2);
                queue.put(3);
                System.out.println("Items added");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();

        // Параллельное извлечение элементов
        new Thread(() -> {
            try {
                System.out.println("Item removed: " + queue.take());
                System.out.println("Item removed: " + queue.take());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();
    }
}
```

### 4. **CopyOnWriteArraySet**

`CopyOnWriteArraySet` — это потокобезопасная версия `HashSet`, которая использует тот же механизм, что и `CopyOnWriteArrayList`. Все изменения (добавление или удаление элементов) приводят к созданию новой копии множества, что делает его подходящим для ситуаций, когда множество часто читается и редко изменяется.

Пример использования `CopyOnWriteArraySet`:

```java
import java.util.concurrent.*;

public class CopyOnWriteArraySetExample {
    public static void main(String[] args) {
        CopyOnWriteArraySet<String> set = new CopyOnWriteArraySet<>();

        set.add("A");
        set.add("B");

        // Параллельное добавление элементов в множество
        new Thread(() -> {
            set.add("C");
            System.out.println("Thread 1: " + set);
        }).start();

        new Thread(() -> {
            set.add("D");
            System.out.println("Thread 2: " + set);
        }).start();
    }
}
```

### 5. **ConcurrentSkipListMap и ConcurrentSkipListSet**

`ConcurrentSkipListMap` и `ConcurrentSkipListSet` — это потокобезопасные коллекции, основанные на структуре данных *skip list* (пропускающий список). Эти коллекции предлагают эффективные операции поиска, вставки и удаления с гарантией логарифмической сложности, а также поддерживают сортировку элементов.

- **ConcurrentSkipListMap** — это потокобезопасная версия `TreeMap`, которая хранит ключи в отсортированном порядке.
- **ConcurrentSkipListSet** — это потокобезопасная версия `TreeSet`, которая хранит элементы в отсортированном порядке.

Пример использования `ConcurrentSkipListMap`:

```java
import java.util.concurrent.*;

public class ConcurrentSkipListMapExample {
    public static void main(String[] args) {
        ConcurrentSkipListMap<Integer, String> map = new ConcurrentSkipListMap<>();

        map.put(1, "one");
        map.put(2, "two");
        map.put(3, "three");

        System.out.println(map);
    }
}
```

### Заключение

Коллекции из пакета `java.util.concurrent` являются важной частью параллельного программирования в Java. Они обеспечивают безопасный доступ и модификацию данных несколькими потоками одновременно, что существенно упрощает разработку многозадачных приложений и повышает их производительность. Основные коллекции, такие как `ConcurrentHashMap`, `CopyOnWriteArrayList`, `BlockingQueue` и другие, предназначены для специфических задач, связанных с параллельной обработкой данных, и позволяют эффективно решать задачи синхронизации и управления потоками.