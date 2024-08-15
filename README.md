# Кодировка H_Code

## Операции
- A, B - числа (могут быть с ведущем 0);
- a, b - отдельные цифры числа;
- A<sub>n</sub> - цифра числа A в позиции n

---

$$A = [a,b,c,d]$$

$$a \tilde+ b=(a + b)\mod10$$

$$a' = (10 - a)\mod10$$

$$a \tilde- b=(a - b + 10)\mod10$$

$$a \tilde-b = a \tilde+ b'$$

---

$$A \tilde+ B = [A_0\tilde+B_0,...,A_n \tilde+ B_n]$$

$$A' = [A_0',...,A_n']$$

$$A \tilde- B = [A_0 \tilde- B_0,...,A_n \tilde- B_n]$$

$$A \tilde- B = A \tilde+ B'$$

---

$$A\xrightarrow{code}X$$

$$X\xrightarrow{decode}A$$

$$A\xrightarrow{code-loop}X$$

$$A\xrightarrow{code-block [key]}X$$

---

## Описание
***Кодирование:***

$$
[1,4,3,2,4]
\xrightarrow{code}
[2,9,9,3,1]$$

***Декодирование:***

$$[2,9,9,3,1]
\xrightarrow{decode}
[1,4,3,2,4]$$

## Циклы
***2-ух значные:***

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
  }_{12}
\xrightarrow{code}[1,3]
$$

$$
\underbrace{
  [0,2]
  \xrightarrow{code}...
  }_{20}
\xrightarrow{code}[0,2]
$$

$$
\underbrace{
  [0,1]
  \xrightarrow{code}...
  }_{60}
\xrightarrow{code}
[0,1]
$$

## Блоки

$$
\begin{CD}
  \begin{matrix}
    5 & 3\\
    4 & 2
  \end{matrix}
  @>c>>
  \begin{matrix}
    8 & 5\\
    8 & 4
  \end{matrix}\\
  @VcVV @VVcV\\
  \begin{matrix}
    9 & 9\\
    5 & 3
  \end{matrix}
  @>c>>
  \begin{matrix}
    0 & 9\\
    9 & 5
  \end{matrix}
\end{CD}
$$

## Использование

```haskell
import HomaCode

-- Test
main :: IO ()
main = do
  putStrLn $ code [1,2,3,4]
```
