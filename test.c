
int main(int argc, char argv) {

    int a = 5 + 2 - 3 * 4;
    int b = a - 5;

    {
        int ablock = a + b - a - b;
    }
}
