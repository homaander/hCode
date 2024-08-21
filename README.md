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

$$
A
\underbrace{
\xrightarrow{code}X\xrightarrow{code}...\xrightarrow{code}
}_{30}
X^{30}_A
==A\xrightarrow{code^{30}}X^{30}_A
$$

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

## Ленты (Циклы кодирования)

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

> Любую ленту можно разложить на любую + какая-то со смещением

$$B_7 = D_0 + B_{47}$$
```
[3,2] -> [9,3] -> [4,9] -> [5,4] -> [9,5]
=
[0,5] -> [5,0] -> [5,5] -> [0,5] -> [5,0]
+
[3,7] -> [4,3] -> [9,4] -> [5,9] -> [4,5]
```

То-есть любую ленту можно разложить как
$$ \_ = B_b + C_c + D_d + E_e + F_f + ...$$

Лентa A не влияет на результат

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

***Правила:***

$$A = X_A\oplus X^2_A$$

**Влияние на все цифры:**
> A<sub>1</sub>: Через 1 кодирование
> A<sub>2</sub>: Через 2 кодирования

***Ленты:***
```
A. [0,0] = 1
B. [0,1] = 60
C. [0,2] = 20
D. [0,5] = 3
E. [1,3] = 12
F. [2,6] = 4
```

*Суммы лент:*

```haskell
findLoop $ sumData (codeN 0 [0,1]) (codeN 0 [0,5])
findCount [0,2] [0,6]
```

|   _|B~0~|C~0~|D~0~|E~0~|F~0~|
|-|-|-|-|-|-|
|B~0~|C~0~|B~15~|C~15~|B~52~|B~24~|
|C~0~|    | C~5~|B~45~|B~41~|C~12~|
|D~0~|    |     | A~0~| E~8~| E~1~|
|E~0~|    |     |     | F~0~| E~3~|
|F~0~|    |     |     |     | F~1~|

## 3-х значные коды

**Влияние на все цифры:**
> A<sub>1</sub>: Через 3 кодирования
> A<sub>2</sub>: Через 2 кодирования
> A<sub>3</sub>: Через 4 кодирования

***Ленты:***
```
A. [0,0,0]: 1
B. [0,0,1]: 434
C. [0,0,2]: 62
D. [0,0,3]: 434
E. [0,0,4]: 62
F. [0,0,5]: 7
```

*Суммы лент:*

|   _|B~0~|C~0~| D~0~|  E~0~|  F~0~|
|----|----|----|-----|------|------|
|B~0~|C~0~|D~0~| E~0~|  F~0~| E~31~|
|C~0~|    |E~0~| F~0~| E~31~|D~217~|
|D~0~|    |    |E~31~|D~217~| C~31~|
|E~0~|    |    |     | C~31~|B~217~|
|F~0~|    |    |     |      |  A~0~|

## 5-х значные коды

**Влияние на все цифры:**

>A<sub>1</sub>: Через 7 кодирования
>A<sub>2</sub>: Через 5 кодирования
>A<sub>3</sub>: Через 4 кодирования
>A<sub>4</sub>: Через 6 кодирования
>A<sub>5</sub>: Через 8 кодирования

***Ленты:***
```
A. [0,0,0,0,0]: 1
B. [0,0,0,0,1]: 24211
C. [0,0,0,0,2]: 781
D. [0,0,0,0,3]: 24211
E. [0,0,0,0,4]: 781
F. [0,0,0,0,5]: 31
G. [0,0,0,0,6]: 781
H. [0,0,0,0,7]: 24211
I. [0,0,0,0,8]: 781
K. [0,0,0,0,9]: 24211
```

## 4-х значные коды

***Ленты:***
```
| [0,0,0,0]: 1   | [0,0,2,6]: 62  | [1,1,0,3]: 62 |
| [0,0,0,1]: 434 | [0,0,2,9]: 434 | [1,1,0,4]: 14 |
| [0,0,0,2]: 62  | [0,0,3,0]: 434 | [1,1,0,5]: 62 |
| [0,0,0,3]: 434 | [0,0,3,2]: 434 | [1,1,0,7]: 62 |
| [0,0,0,4]: 62  | [0,0,3,4]: 434 | [1,1,0,9]: 2  |
| [0,0,0,5]: 7   | [0,0,3,6]: 434 | [1,1,2,5]: 62 |
| [0,0,1,0]: 434 | [0,0,4,0]: 62  | [1,1,4,1]: 62 |
| [0,0,1,2]: 434 | [0,0,4,2]: 62  | [1,1,4,7]: 62 |
| [0,0,1,3]: 434 | [0,0,4,8]: 62  | [1,1,4,9]: 62 |
| [0,0,1,4]: 434 | [0,0,5,0]: 7   | [1,1,5,9]: 14 |
| [0,0,1,5]: 434 | [0,1,2,4]: 434 | [1,1,6,1]: 62 |
| [0,0,1,7]: 434 | [0,1,2,9]: 434 | [1,1,6,9]: 62 |
| [0,0,1,8]: 434 | [0,1,3,2]: 434 | [2,2,0,3]: 14 |
| [0,0,2,0]: 62  | [0,1,3,7]: 434 | [2,2,0,8]: 2  |
| [0,0,2,1]: 434 | [0,2,4,8]: 62  | [2,2,5,8]: 14 |
| [0,0,2,4]: 62  | [0,2,6,4]: 62  | [3,3,0,7]: 2  |
| [0,0,2,5]: 434 | [1,1,0,1]: 62  | [4,4,0,6]: 2  |
                                    [5,5,0,5]: 1
```

## Использование

```haskell
import HomaCode

-- Test
main :: IO ()
main = do
  putStrLn $ code [1,2,3,4]
```
