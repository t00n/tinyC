
int quicksort(int array[1000], int beg, int end) {
    if (end - beg > 0) {
        int middle = (end + beg) / 2;
        int pivot = array[middle];
        int b = beg;
        int e = end;
        while (b < e) {
            int tmp;
            while (array[b] < pivot) { b = b + 1; }
            while (array[e] > pivot) { e = e - 1; }
            if (b < e) {
                tmp = array[b];
                array[b] = array[e];
                array[e] = tmp;
                b = b + 1;
                e = e - 1;
            }
            else if (b == e) {
                tmp = array[b];
                array[b] = array[e];
                array[e] = tmp;
                b = b + 1;
                e = e - 1;
            }
        }
        quicksort(array, beg, middle);
        quicksort(array, middle+1, end);
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
    while (i < length a) {
        write a[i];
        i = i + 1;
    }
    i = 0;
    quicksort(a, 0, 4);
    while (i < length a) {
        write a[i];
        i = i + 1;
    }
}