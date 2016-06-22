

int tiny() {
    int * a;
    int x = 5;
    int * b;
    int xs[5];
    xs[0] = 10;
    xs[1] = 15;
    xs[2] = 20;
    b = &xs[2];
    write *b;
    a = b;
    *a = 5;
    b = a;
    write *b;
    b = xs;
    write *b;
    write b[1];
}