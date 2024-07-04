/* errorlocate */

/* Objective function */
min: +1.32323 E_delta_a +1.4232 E_delta_b;

/* Constraints */
V1: +a_A1 +a_A2 = 1;
V2: -a_A1 +b <= 0;
a: +a_A2 +E_delta_a = 1;
b: +b +E_delta_b = 1;

/* Variable bounds */
a_A1 <= 1;
a_A2 <= 1;
b <= 1;
E_delta_a <= 1;
E_delta_b <= 1;

/* Integer definitions */
bin a_A1,a_A2,b,E_delta_a,E_delta_b;

/* SOS definitions */
SOS
a: a_A1,a_A2:1 <= 1;
