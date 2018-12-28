#include <stdio.h>

#define MAX_VALUE 16777215
#define SIZE (16777215 / (8 * sizeof(unsigned char)))

long a, b, c, d, e, f;

long last;

unsigned char values_set[SIZE];

void reset() {
  for (int i = 0; i < SIZE; i++) {
    values_set[i] = 0;
  }
}

void set(long i) {
  int idx = i / 8;
  int bit = i % 8;

  values_set[idx] |= 1 << bit;
}

int is_set(long i) {
  int idx = i / 8;
  int bit = i % 8;

  return values_set[idx] & (1 << bit);
}

void println(char *msg) {
  printf("%s: %ld %ld %ld %ld %ld %ld\n", msg, a, b, c, d, e, f);
}

/*
 * 0: seti 123 0 1
 * 1: bani 1 456 1
 * 2: eqri 1 72 1
 * 3: addr 1 2 2
 * 4: seti 0 0 2
 * 5: seti 0 9 1
 * 6: bori 1 65536 4
 * 7: seti 16298264 8 1
 * 8: bani 4 255 5
 * 9: addr 1 5 1
 * 10: bani 1 16777215 1
 * 11: muli 1 65899 1
 * 12: bani 1 16777215 1
 * 13: gtir 256 4 5
 * 14: addr 5 2 2
 * 15: addi 2 1 2
 * 16: seti 27 1 2
 * 17: seti 0 3 5
 * 18: addi 5 1 3
 * 19: muli 3 256 3
 * 20: gtrr 3 4 3
 * 21: addr 3 2 2
 * 22: addi 2 1 2
 * 23: seti 25 4 2
 * 24: addi 5 1 5
 * 25: seti 17 1 2
 * 26: setr 5 3 4
 * 27: seti 7 7 2
 * 28: eqrr 1 0 5
 * 29: addr 5 2 2
 * 30: seti 5 3 2
 */
void solve(char* msg, long value, int s) {
  int count = 0;
  
  reset();

  // a=0 b=1 c=2 (pc) d=3 e=4 f=5

  a = value;
  b = 0;
  c = 0;
  d = 0;
  e = 0;
  f = 0;

  b = 123;
 label1:
  b &= 456;
  b = b == 72;
  if (b == 0)
    goto label1;
  b = 0;
 label6:
  e = b | 65536L;
  b = 16298264L;
 label8:
  f = e & 255;
  b += f;
  b &= 16777215L;
  b *= 65899L;
  b &= 16777215L;
  f = 256 > e;
  if (f == 1)
    goto label28;
  f = 0;
 label18:
  d = f + 1;
  d *= 256;
  d = d > e;
  if (d == 1)
    goto label26;
  f += 1;
  goto label18;
 label26:
  e = f;
  goto label8;
 label28:
  f = b == a;
  if (f == 1)
    goto out;
  {
    if (s)
      goto out;
    else if (is_set(b))
      goto out;
    else {
      set(b);
      last = b;
      count++;
      if (count % 1000 == 0) {
	char buffer[1024];

	snprintf(buffer, 1024, "checkpoint %d", count);
	
	println(buffer);
      }
    }
  }
  goto label6;

 out:
  return;
}

int main(int args, char *argv[]) {
  solve("solution 1", 0, 1);

  printf("solution 1: %ld\n", b);

  solve("solution 2", 0, 0);

  printf("solution 2: %ld\n", last);
  
  return 0;
}
