package ip.ivanov.danil.Практика.Задание10;

public class HelloParallel {

    public static void main(String[] args) {
        // Запуск 5 параллельных потоков
        for (int i = 1; i <= 5; i++) {
            int processNumber = i;
            new Thread(() -> printHello(processNumber)).start();
        }
    }

    // Функция для вывода сообщения с задержкой
    private static void printHello(int processNumber) {
        for (int i = 1; i <= 5; i++) {
            try {
                // Выводим сообщение
                System.out.println("Hello from " + processNumber + "!");
                // Задержка 1 секунда
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
