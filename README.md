# Кодировка H_Code

## Операции
- A, B - числа в виде списка элементов
- a, b - отдельные цифры числа
- A<sub>n</sub> - цифра числа A в позиции n

---

$$A = [A_0,A_1,A_2,A_3] = [a,b,c,d]$$

$$a \oplus b=(a + b)\bmod10$$

$$a' = (10 - a)\bmod 10$$

$$a \ominus b=(a - b + 10)\bmod10$$

$$a \ominus b = a\oplus b'$$

---

$$A \oplus B = [A_0\oplus B_0,...,A_n\oplus B_n]$$

$$A' = [A_0',...,A_n']$$

$$A \ominus B = [A_0 \ominus B_0,...,A_n \ominus B_n]$$

$$A \ominus B = A\oplus B'$$

---

$$A\xrightarrow{code}X_A$$

$$X_A\xrightarrow{decode}A$$

$$X_A\oplus X_B=X_{A\oplus B}$$

$$X_A\ominus X_B=X_{A\ominus B}$$

---

$$A\xrightarrow{code^{30}}X^{30}_A$$

$$A\xrightarrow{code^A}L_A$$

---

$$A\xrightarrow{cBlock(x,y[,key])}B_A$$

---

## Описание
***Кодирование:***


$$
[1,4,3,2,4]
\xrightarrow{code}
[2,9,9,3,1]
$$

$$
\text{reverse }([1,4,3,2,4]\ominus[0,1,4,3,2,\cancel4]) = [2,9,9,3,1]
$$

$$
\text{reverse }[(1\ominus0),(4\ominus1),(3\ominus4),(2\ominus3),(4\ominus2)] = [2,9,9,3,1]
$$

***Декодирование:***

$$
[2,9,9,3,1]
\xrightarrow{decode}
[1,4,3,2,4]
$$

$$[0,0,0,0,2]\oplus[0,0,0,9,9]\oplus[0,0,9,9,9]\oplus[0,3,3,3,3]\oplus[1,1,1,1,1]=[1,4,3,2,4]$$

## Циклы

## Блоки

$$
\begin{CD}
  \begin{array}{c|c}
    5 & 3\\
    \hline
    4 & 2
  \end{array}
  @>>>
  \begin{array}{c|c}
    8 & 5\\
    \hline
    8 & 4
  \end{array}\\
  @VVV @VVV\\
  \begin{array}{c|c}
    9 & 9\\
    \hline
    5 & 3
  \end{array}
  @>>>
  \begin{array}{c|c}
    0 & 9\\
    \hline
    8 & 5
  \end{array}
\end{CD}
$$

## 2-ух значные коды

***Правила***
$$A = X_A\oplus X^2_A$$

***Циклы:***
$$
\underbrace{[0,0]}_{1}
\xrightarrow{code}[0,0]
$$

$$
\underbrace{
  [0,5]
  \xrightarrow{code}[5,0]
  \xrightarrow{code}[5,5]
  }_3
\xrightarrow{code}
[0,5]
$$

$$\underbrace{
  [2,6]
  \xrightarrow{code}[4,2]
  \xrightarrow{code}[8,4]
  \xrightarrow{code}[6,8]
  }_4
\xrightarrow{code}
[2,6]
$$

$$\underbrace{
  [1,3]
  \xrightarrow{code}...
  \xrightarrow{code}[3,4]
  }_{12}
\xrightarrow{code}[1,3]
$$

$$
\underbrace{
  [0,2]
  \xrightarrow{code}...
  \xrightarrow{code}[2,2]
  }_{20}
\xrightarrow{code}[0,2]
$$

$$
\underbrace{
  [0,1]
  \xrightarrow{code}...
  \xrightarrow{code}[1,1]
  }_{60}
\xrightarrow{code}
[0,1]
$$

## Использование

```haskell
import HomaCode

-- Test
main :: IO ()
main = do
  putStrLn $ code [1,2,3,4]
```
