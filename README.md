###Implement simple polynomial arithmetic in Lisp

1. There are 4 functions available: poly+, poly-, poly*, test
  - all of them takes exactly two arguments
  - 'poly+', 'poly-' and 'poly*' return results of adding, subtracting or multiplication of two input polynomials
  - 'test' prints out the results of 'poly+', 'poly-' and 'poly*'
2. Polynomial format:
  - Representation of terms in polynomial:
    - Each term should be represented like this: ```(coefficient letter exponent)```
  - You can put as many letters and exponents as you want.
    - ```X^2``` --> ```(1 X 2)```
    - ```3XY^2Z``` --> ```(3 X 1 Y 2 Z 1)```
  - Put a simplified term:
    - ```XX^2``` should rather be ```X^3```
    - ```X^(1+2)``` should rather be ```X^3```
3. Format of polynomials:
  - Polynomials should be a form of lists contains terms.
  - '+' and '-' symbols between terms should be represent by the coefficient of the second terms:
    - ```X + Y``` --> ```((1 X 1) (1 Y 1))```
    - ```2XY - Z^2``` --> ```((2 X 1 Y 1) (-1 Z 2))```
  - '*' and "/' symbols between terms should be simplified first:
    - ```XY * XYZ``` --> ```X^2Y^2Z``` --> ```(1 X 2 Y 2 Z 1)```
    - ```XYZ / XY``` --> ```Z``` --> ```(1 Z 1)```
4. There is not validation of inputs, so make sure the inputs are correctly formatted.
5. How to test:
  - Run clisp
  - Load this file: (load "assignment1.lisp")
  - Call test by passing 2 polynomials: P1 P2
  - Results are showed by this order:
    - P1 + P2
    - P2 - P2
    - P1 * P2
  - You can also test poly+, poly-, poly* individually.
