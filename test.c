
int main(void) {
  int i = 5;
  i = i < 5 ? i:i;

  if (i == 5) {
    printf("%d\n", i);
  }

  if (i != (i==2? i+3 : i+4)) {
  }
}

void someFunction(void) {
  float something = 4;
}
