#include <stdio.h>
#include <stdlib.h>

#define SIZE 100000

typedef struct sf_node {
   int sf[6];
   struct sf_node *next;
} sf_node;

int code(int sf[]) {
   int sum = 0;
   for (int i=0; i<6; i++)
      sum += sf[i];
   return sum % SIZE;
}

int i_right(int sf1[], int sf2[], int start) {
   for (int i=0; i<6; i++) {
      if (sf1[i] != sf2[(start+i) % 6])
         return 0;
   }
   return 1;
}

int i_left(int sf1[], int sf2[], int start) {
   int sf2_i = 0;
   for (int i=0; i<6; i++) {
      sf2_i = start - i;
      if (sf2_i < 0) sf2_i += 6;
      if (sf1[i] != sf2[sf2_i])
         return 0;
   }
   return 1;
}

int are_ident(int sf1[], int sf2[]) {
   for (int i=0; i<6; i++) {
      if (i_right(sf1, sf2, i))
         return 1;
      if (i_left(sf1, sf2, i))
         return 1;
   }
   return 0;
}

void find_ident(sf_node *sfs[]) {
   sf_node *node1, *node2;
   for (int i=0; i<SIZE; i++) {
      node1 = sfs[i];
      while (node1 != NULL) {
         node2 = node1->next;
         while (node2 != NULL) {
            if (are_ident(node1->sf, node2->sf)) {
               printf("Twin snowflakes found.\n");
               return;
            }
            node2 = node2->next;
         }
         node1 = node1->next;
      }
   }
   printf("No two snowflakes are alike.\n");
}

int main(void) {
   static sf_node *sfs[SIZE] = {NULL};
   sf_node *sf;
   int n, sf_code = 0;
   scanf("%d", &n);
   for (int i=0; i<n; i++) {
      sf = malloc(sizeof(sf_node));
      for (int j=0; j<6; j++)
         scanf("%d", &sf->sf[j]);
      sf_code = code(sf->sf);
      sf->next = sfs[sf_code];
      sfs[sf_code] = sf;
   }
   find_ident(sfs);
   return 0;
}