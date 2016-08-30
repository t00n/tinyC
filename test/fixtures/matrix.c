
int matrix[4][4];
int vector[4];
int result[4];

int matrix_id(int m[4][4]) {
	int i = 0;
	int j = 0;
	while (i < 4) {
		j = 0;
		while (j < 4) {
			if (i == j) {
				m[i][j] = 1;
			}
			else {
				m[i][j] = 0;
			}
			j = j + 1;
		}
		i = i + 1;
	}
}

int m_v_mult(int a[4][4], int d[4], int c[4]) {
	int i = 0;
	int j = 0;
	while (i < 4) {
		c[i] = 0;
		j = 0;
		while (j < 4) {
			c[i] = c[i] + a[i][j] * d[j];
			j = j + 1;
		}
		i = i + 1;
	}
}

int write_vector(int v[4]) {
	int i = 0;
	while (i < 4) {
		write v[i];
		write '|';
		i = i + 1;
	}
}

int write_matrix(int m[4][4]) {
	int i = 0;
	int j;
	while (i < 4) {
		j = 0;
		while (j < 4) {
			write m[i][j];
			write '|';
			j = j + 1;
		}
		i = i + 1;
	}
}

int tiny() {
	int i = 0;
	int j = 0;
	matrix_id(matrix);
	vector[0] = 2;
	vector[1] = 3;
	vector[2] = 1;
	vector[3] = 4;
	write_vector(vector);
	m_v_mult(matrix, vector, result);
	write_vector(result);
}