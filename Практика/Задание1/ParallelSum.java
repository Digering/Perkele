package ip.ivanov.danil.Практика.Задание1;

import java.util.concurrent.*;

public class ParallelSum {

    private static final double START = 0;
    private static final double END = 6.28;
    private static final double STEP = 0.0000001;

    public static void main(String[] args) throws InterruptedException, ExecutionException {
        // Создаем пул потоков
        ExecutorService executor = Executors.newFixedThreadPool(2);

        // Сначала вычисляем первую функцию в одном потоке
        double sum1 = calculateSum(START, END, STEP, x -> 34 * x + 0.098);
        System.out.println("Сумма функции 1 (y = 34x + 0.098): " + sum1);

        // Задачи для параллельного вычисления оставшихся функций
        Callable<Double> task2 = () -> calculateSum(START, END, STEP, x -> Math.sin(x) + 45 * Math.pow(x, 2));
        Callable<Double> task3 = () -> calculateSum(START, END, STEP, x -> 1 / (x + 1));

        // Запуск задач
        Future<Double> result2 = executor.submit(task2);
        Future<Double> result3 = executor.submit(task3);

        // Получение результатов
        double sum2 = result2.get();
        double sum3 = result3.get();

        // Закрытие пула потоков
        executor.shutdown();

        // Вычисление среднего значения
        double average = (sum1 + sum2 + sum3) / 3;

        // Вывод результатов
        System.out.println("Сумма функции 2 (y = sin(x) + 45x^2): " + sum2);
        System.out.println("Сумма функции 3 (y = 1 / (x + 1)): " + sum3);
        System.out.println("Среднее значение: " + average);
    }

    // Метод для вычисления суммы значений функции
    private static double calculateSum(double start, double end, double step, java.util.function.Function<Double, Double> function) {
        double sum = 0;
        for (double x = start; x <= end; x += step) {
            sum += function.apply(x);
        }
        return sum;
    }
}
