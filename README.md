# Кодировка H_Code

## Операции
- A, B - числа в виде списка элементов
- a, b - отдельные цифры числа
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

$$A\xrightarrow{code}X_A$$

$$X_A\xrightarrow{decode}A$$

$$X_A\tilde+X_B=X_{A\tilde+B}$$

$$X_A\tilde-X_B=X_{A\tilde-B}$$

---

$$A\xrightarrow{cLoop}L_A$$

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
reverse[(1\tilde-0),(4\tilde-1),(3\tilde-4),(2\tilde-3),(4\tilde-2)] = [2,9,9,3,1]
$$

***Декодирование:***

$$
[2,9,9,3,1]
\xrightarrow{decode}
[1,4,3,2,4]
$$

$$[0,0,0,0,2]\tilde+[0,0,0,9,9]\tilde+[0,0,9,9,9]\tilde+[0,3,3,3,3]\tilde+[1,1,1,1,1]=[1,4,3,2,4]$$

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
    9 & 5
  \end{array}
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
