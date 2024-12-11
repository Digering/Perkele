package ip.ivanov.danil.Практика.Задание12;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.DoubleUnaryOperator;

public class ParallelFunctionSum {

    public static void main(String[] args) throws Exception {
        double start = 0.0;       // Начало интервала
        double end = 3.14;       // Конец интервала
        double step = 0.0000001; // Шаг

        // Пул потоков
        ExecutorService executor = Executors.newFixedThreadPool(2);

        // Первая группа потоков
        Future<Double> sum1 = executor.submit(() -> calculateSum(start, end, step, x -> Math.cos(x) + 45 * x * x));
        Future<Double> sum2 = executor.submit(() -> calculateSum(start, end, step, x -> 1 / (x + 1)));

        // Ожидаем завершения первой группы
        double result1 = sum1.get();
        double result2 = sum2.get();

        // Вторая группа потоков
        Future<Double> sum3 = executor.submit(() -> calculateSum(start, end, step, x -> 34 * x + 0.098));
        Future<Double> sum4 = executor.submit(() -> calculateSum(start, end, step, x -> 17 * x + 9.8));

        // Ожидаем завершения второй группы
        double result3 = sum3.get();
        double result4 = sum4.get();

        // Завершаем пул потоков
        executor.shutdown();

        // Вычисляем среднее значение всех сумм
        double average = (result1 + result2 + result3 + result4) / 4;

        // Выводим результаты
        System.out.println("Sum 1 (cos(x) + 45x^2): " + result1);
        System.out.println("Sum 2 (1/(x+1)): " + result2);
        System.out.println("Sum 3 (34x + 0.098): " + result3);
        System.out.println("Sum 4 (17x + 9.8): " + result4);
        System.out.println("Average of all sums: " + average);
    }

    /**
     * Метод для вычисления суммы значений функции на заданном интервале.
     */
    private static double calculateSum(double start, double end, double step, DoubleUnaryOperator function) {
        double sum = 0.0;
        for (double x = start; x <= end; x += step) {
            sum += function.applyAsDouble(x);
        }
        return sum;
    }
}
