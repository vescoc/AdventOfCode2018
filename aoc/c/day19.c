#include <stdio.h>

long a, b, c, d, e, f;

void print(char* msg) {
  printf("%s: %ld %ld %ld %ld %ld %ld\n", msg, a, b, c, d, e, f);      
}

void solve(char* msg, int value) {
  a = value;
  b = 0;
  c = 0;
  d = 0;
  e = 0;
  f = 0;

  /*
    |0=a, 1=b, 2=c, 3=d, 4=e, 5=f
    |#ip 2
    |0: addi 2 16 2 | jp 0 + 16 + 1 (17)
    |1: seti 1 0 1 | b = 1
    |2: seti 1 4 3 | d = 1
    |3: mulr 1 3 4 | e = b * d
    |4: eqrr 4 5 4 | e = if e == f
    |5: addr 4 2 2 | jp e + 5 + 1 (6 / 7 e)
    |6: addi 2 1 2 | jp 6 + 1 + 1 (8)
    |7: addr 1 0 0 | a = b + a
    |8: addi 3 1 3 | d = d + 1
    |9: gtrr 3 5 4 | e = if d > f
    |10: addr 2 4 2 | jp 10 + e + 1 (11 / 12 e)
    |11: seti 2 5 2 | jp 2 + 1 (3)
    |12: addi 1 1 1 | b = b + 1
    |13: gtrr 1 5 4 | e = if b > f
    |14: addr 4 2 2 | jp e + 14 + 1 (15 / 16 e)
    |15: seti 1 1 2 | jp 1 + 1 (2)
    |16: mulr 2 2 2 | jp 16 * 16 + 1
    |17: addi 5 2 5 | f = f + 2
    |18: mulr 5 5 5 | f = f * f
    |19: mulr 2 5 5 | f = 19 * f
    |20: muli 5 11 5 | f = f * 11
    |21: addi 4 5 4 | e = e + 5
    |22: mulr 4 2 4 | e = e * 22
    |23: addi 4 9 4 | e = e + 9
    |24: addr 5 4 5 | f = f + e
    |25: addr 2 0 2 | jp 25 + a + 1 (26 + a)
    |26: seti 0 0 2 | jp 0 + 1 (1)
    |27: setr 2 3 4 | e = 27 + d
    |28: mulr 4 2 4 | e = e * 28
    |29: addr 2 4 4 | e = 29 + e
    |30: mulr 2 4 4 | e = 30 * e
    |31: muli 4 14 4 | e = e * 14
    |32: mulr 4 2 4 | e = 32 * e
    |33: addr 5 4 5 | f = f + e
    |34: seti 0 6 0 | a = 0
    |35: seti 0 3 2 | jp 0 + 1 (1)
  */
  
  goto label17;
 label1:
  print("label1");
  b = 1;
 label2:
  d = 1;
 label3:
  e = b * d;
  e = e == f;
  if (e == 0)
    goto label8;
  print("b + a");
  a = b + a;
 label8:
  d = d + 1;
  e = d > f;
  if (e == 0)
    goto label3;
  b = b + 1;
  e = b > f;
  if (e == 0)
    goto label2;
  else
    goto out;
 label17:
  f = f + 2;
  f = f * f;
  f = 19 * f;
  f = f * 11;
  e = e + 5;
  e = e * 22;
  e = e + 9;
  f = f + e;
  if (a == 0)
    goto label1;
  if (a <= 1)
    e = 27 + d;
  if (a <= 2)
    e = e * 28;
  if (a <= 3)
    e = 29 + e;
  if (a <= 4)
    e = 30 * e;
  if (a <= 5)
    e = e * 14;
  if (a <= 6)
    e = 32 * e;
  if (a <= 7)
    f = f + e;
  if (a <= 8)
    a = 0;
  if (a <= 9)
    goto label1;
 out:
  print(msg);
}

int main(int argc, char* argv[]) {
  solve("solution 1", 0);
  solve("solution 2", 1);
}
