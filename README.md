# Кодировка H_Code

### Операции
> A, B - числа (могут быть с ведущем 0)
> a, b - отдельные цифры числа
> A~n~ - цифра числа A в позиции n


$$ab | cd = abcd$$

$$a[+]b=(a + b)\mod10$$

$$a' = (10 - a)\mod10$$

$$a[-]b=(a - b + 10)\mod10$$

$$a[-]b = a[+]b'$$

---

$$A\ [+]\ B = (A_0 [+] B_0)|\ ...\ |\ (A_n [+] B_n)$$

$$A' = A_0'|\ ...\ |A_n'$$

$$A\ [-]\ B = A_0 [-] B_0|...|A_n [-] B_n$$

$$A\ [-]\ B = A\ [+]\ B'$$

---

$$A\xrightarrow{code}X$$

$$X\xrightarrow{decode}A$$

$$A\xRightarrow{code-loop}X$$

$$A\xRightarrow{code-block [key]}X$$

### Описание
***Кодирование:***

$$14324\xrightarrow{code}29931$$

***Декодирование:***

$$29931\xrightarrow{decode}14324$$

### Циклы
***2х значные***

$$\underbrace{00}_1\xrightarrow{code}00$$

$$\underbrace{05\xrightarrow{code}50\xrightarrow{code}55}_3\xrightarrow{code}05$$

$$\underbrace{26\xrightarrow{code}42\xrightarrow{code}84\xrightarrow{code}68}_4\xrightarrow{code}26$$

$$\underbrace{13\xrightarrow{code}...}_{12}\xrightarrow{code}13$$

$$\underbrace{02\xrightarrow{code}...}_{20}\xrightarrow{code}02$$

$$\underbrace{01\xrightarrow{code}...}_{60}\xrightarrow{code}01$$

### Блоки

### Использование

```haskell
-- Test
a :: IO ()
a = do
  putStrLn "Hello"
```
