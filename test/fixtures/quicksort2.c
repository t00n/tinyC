
int swap_ptr(int * a, int * b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int quicksort(int * beg, int * end) {
    if (end - beg > 0) {
        int * middle = (end - beg) / 2 + beg;
        int pivot = *middle;
        int * b = beg;
        int * e = end;
        while (b < e) {
            int tmp;
            while (*b < pivot) { b = b + 4; }
            while (*e > pivot) { e = e - 4; }
            if (b < e) {
                swap_ptr(b, e);
            }
            else if (b == e) {
                swap_ptr(b, e);
            }
        }
        quicksort(beg, middle);
        quicksort(middle+4, end);
    }
}

int tiny() {
    int a[5];
    int i = 0;
    a[0] = 5;
    a[1] = 2;
    a[2] = 3;
    a[3] = 1;
    a[4] = 4;
    while (i < 5) {
        write a[i];
        i = i + 1;
    }
    i = 0;
    quicksort(a, &a[4]);
    while (i < 5) {
        write a[i];
        i = i + 1;
    }
}