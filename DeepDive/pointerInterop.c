struct myType {
  int length;
  double * temperature;
};

extern void convertFtoC(struct myType * myTypeF, struct myType * myTypeC);

int main() {
  struct myType myTypeF;
  struct myType myTypeC;

  myTypeF.length = 10;
  myTypeF.temperature = (double *) malloc(myTypeF.length * sizeof(double));

  convertFtoC(&myTypeF, &myTypeC);

  free(myTypeF.temperature);

  deinit_myType(&myTypeC);

  return 0;
}