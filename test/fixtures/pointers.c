
int * a;

int tiny() {
    int * b;
    int xs[5];
    xs[0] = 10;
    xs[1] = 15;
    *a = 5;
    b = a;
    write *b;
    b = xs;
    write *b;
    write b[1];
}