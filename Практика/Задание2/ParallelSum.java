package ip.ivanov.danil.Практика.Задание2;

import java.util.concurrent.*;
import java.util.function.Function;

public class ParallelSum {

    private static final double START = 0;
    private static final double END = 3.14;
    private static final double STEP = 0.0000001;

    public static void main(String[] args) throws InterruptedException, ExecutionException {
        // Создаем пул потоков
        ExecutorService executor = Executors.newFixedThreadPool(2);

        // Задачи для параллельного вычисления первых двух функций
        Callable<Double> task1 = () -> calculateSum(START, END, STEP, x -> Math.cos(x) + 45 * Math.pow(x, 2));
        Callable<Double> task2 = () -> calculateSum(START, END, STEP, x -> 1 / (x + 1));

        // Запуск задач
        Future<Double> result1 = executor.submit(task1);
        Future<Double> result2 = executor.submit(task2);

        // Получение результатов первых двух функций
        double sum1 = result1.get();
        double sum2 = result2.get();

        // Закрытие пула потоков
        executor.shutdown();

        // Вычисление третьей функции в одном потоке
        double sum3 = calculateSum(START, END, STEP, x -> 34 * x + 0.098);

        // Вычисление среднего значения
        double average = (sum1 + sum2 + sum3) / 3;

        // Вывод результатов
        System.out.println("Сумма функции 1 (y = cos(x) + 45x^2): " + sum1);
        System.out.println("Сумма функции 2 (y = 1 / (x + 1)): " + sum2);
        System.out.println("Сумма функции 3 (y = 34x + 0.098): " + sum3);
        System.out.println("Среднее значение: " + average);
    }

    // Метод для вычисления суммы значений функции
    private static double calculateSum(double start, double end, double step, Function<Double, Double> function) {
        double sum = 0;
        for (double x = start; x <= end; x += step) {
            sum += function.apply(x);
        }
        return sum;
    }
}
