
int a = 5;
int b = 2; 
int c = 4;
int d = 3;

int tiny() {
    write ((a * b) / (b - c)) / ((d * a) + c * b) + (a * c) / (b - d);
}
// ((5 * 2) / (2 - 4)) / ((3 * 5) + 4 * 2) + (5 * 4) / (2 - 3)
// = -5 / 23 + 20 / - 1
// = 0 - 20 = - 20