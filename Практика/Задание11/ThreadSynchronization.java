package ip.ivanov.danil.Практика.Задание11;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class ThreadSynchronization {

    private static final int NUM_THREADS = 3; // Количество потоков
    private static final int INCREMENT_COUNT = 1000000; // Количество инкрементов на каждый поток
    private static int N = 0; // Обычная переменная для хранения значения N

    public static void main(String[] args) throws InterruptedException, ExecutionException {
        // Создаем ExecutorService для управления потоками
        ExecutorService executor = Executors.newFixedThreadPool(NUM_THREADS);

        // Запускаем 3 потока с помощью ExecutorService
        List<Callable<Void>> tasks = new ArrayList<>();
        for (int i = 0; i < NUM_THREADS; i++) {
            tasks.add(() -> {
                for (int j = 0; j < INCREMENT_COUNT; j++) {
                    increment(); // Увеличиваем значение N на 1
                }
                return null;
            });
        }

        // Выполняем все задачи
        executor.invokeAll(tasks);

        // Завершаем работу с пулом потоков
        executor.shutdown();

        // После завершения всех потоков выводим итоговое значение переменной N
        System.out.println("Final value of N: " + N);
    }

    // Синхронизированный метод для увеличения значения N
    private synchronized static void increment() {
        N++; // Увеличиваем значение N на 1
    }
}
