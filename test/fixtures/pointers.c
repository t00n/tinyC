

int tiny() {
    int * a;
    int x = 5;
    int * b;
    int xs[5];
    xs[0] = 10;
    xs[1] = 15;
    b = &xs[2];
    a = b;
    *a = 5;
    *a = x;
    write *b;
    b = xs;
    write *b;
    write b[1];
}