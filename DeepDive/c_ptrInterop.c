#include <stdio.h>
#include <stdlib.h>
struct wrapperType {
  void * ptr;
};

extern void initWrapper(struct wrapperType * wrapper, int * length);
extern void setTemperatureArray(struct wrapperType * wrapper, int * length, double * temperature);
extern void getTemperatureArray(struct wrapperType * wrapper, int * length, double * temperature);
extern void convertFtoC(struct wrapperType * wrapperF, struct wrapperType * wrapperC);
extern void deinitWrapper(struct wrapperType * wrapper);

int main() {
  struct wrapperType wrapperF;
  struct wrapperType wrapperC;

  int size = 10;
  initWrapper(&wrapperF, &size);
  initWrapper(&wrapperC, &size);

  // We can still use arrays as arguments to Fortran interop functions
  double * temperature = (double *) malloc(10 * sizeof(double));
  for(int i = 0; i < 10; i++) {
    temperature[i] = 32 + 0.5*i;
  }
  setTemperatureArray(&wrapperF, &size, temperature);
  
  getTemperatureArray(&wrapperF, &size, temperature);
  printf("Fahrenheit: ");
  for(int i=0 ; i < 10; i++) {
    printf("%f ", temperature[i]);
  }
  printf("\n");

  convertFtoC(&wrapperF, &wrapperC);

  getTemperatureArray(&wrapperC, &size, temperature);
  printf("Celsius: ");
  for(int i=0 ; i < 10; i++) {
    printf("%f ", temperature[i]);
  }
  free(temperature);

  deinitWrapper(&wrapperF);
  deinitWrapper(&wrapperC);

  return 0;
}