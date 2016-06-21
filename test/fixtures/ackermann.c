int ackermann(int m, int n) {
    if (m < 0) {
        return -1;
    }
    if (n < 0) {
        return -1;
    }
    if (m == 0) {
        return n + 1;
    }
    else {
        if (n == 0) {
            return ackermann(m - 1, 1);
        }
        else {
            return ackermann(m - 1, ackermann(m, n - 1));
        }
    }
}

int tiny() {
    int i = 0;
    int j = 0;
    while (i < 3) {
        j = 0;
        while (j < 3) {
            write ackermann(i, j);
            j = j + 1;
        }
        i = i + 1;
    }
}