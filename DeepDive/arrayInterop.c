#include <stdlib.h>
#include <stdio.h>
struct myType {
  int year;
  double temperature[20];
};

extern void convertFtoC(struct myType * myTypeF, struct myType * myTypeC);

int main() {
  struct myType myTypeF;
  struct myType myTypeC;

  myTypeF.year = 2025;
  for(int i = 0; i < 20; i++) {
    myTypeF.temperature[i] = 32 + 0.5*i;
  }

  convertFtoC(&myTypeF, &myTypeC);

  printf("Fahrenheit: ");
  for(int i=0 ; i < 20; i++) {
    printf("%f ", myTypeF.temperature[i]);
  }

  printf("\nCelsius: ");
  for(int i=0 ; i < 20; i++) {
    printf("%f ", myTypeC.temperature[i]);
  }
  printf("\n");
  return 0;
}