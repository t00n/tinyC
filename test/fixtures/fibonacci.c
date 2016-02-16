
int fibonacci(int n) {
    if (n < 0) {
        return -1;
    }
    else if (n < 1) {
        return 0;
    }
    else if (n < 2) {
        return 1;
    }
    else {
        return fibonacci(n-1) + fibonacci(n-2);
    }
}

int main(int argc, char** argv) {
    int i = 0;
    while (i < 10) {
        // printf("%i\n", fibonacci(i));
        write(fibonacci(i));
        i = i + 1;
    }
}