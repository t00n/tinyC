
int modulo(int x, int y) {
    if (y == 0) {
        return x;
    }
    else {
        int div = x / y;
        int mul = div * y;
        return x - mul;
    }
}

int euclides1(int a, int b) {
    while (b != 0) {
        int tmp = b;
        b = modulo(a, b);
        a = tmp;
    }
    return a;
}

int euclides2(int a, int b) {
    while (a != b) {
        if (a > b) {
            a = a - b;
        }
        else {
            b = b - a;
        }
    }
    return a;
}

int tiny() {
    write euclides1(54, 24);
    write euclides1(42, 56);
    write euclides1(17, 13);
    write euclides2(54, 24);
    write euclides2(42, 56);
    write euclides2(17, 13);
}