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

При кодировании мы получаем новое число, той же разрядности (длинны), и рано или поздно при множестве кодирования мы придем к изначальному числу

**Пример одной из лент 2-х значной кодировки:**

$$\underbrace{
  [2,6]
  \xrightarrow{code}[4,2]
  \xrightarrow{code}[8,4]
  \xrightarrow{code}[6,8]
  }_4
\xrightarrow{code}
[2,6]
$$

Есть 2 ленты, которые есть во всех разрядностях, и их функции можно использовать:
1. Лента нулей, Всегда обозначается (0) и имеет 1 элемент. никак не влияет на другие ленты при сложении
2. Лента состоящая только из 0 и 5. можно использовать как инвертор отдельных эдементов кода (При сумме и разности даёт один и тот-же результат: что-то инвертирует)

> Любую ленту можно разложить сумму одной любой ленты и другой (со смещением)

Например, У нас есть лента (1)<sub>7</sub> и мы хотим представить её суммой (5)<sub>0</sub> и чего-то еще

$$(1)_7 = (5)_0 \oplus x$$

$$[3,2]... = [5,0]... \oplus x$$

Что-бы найти недостающую часть, мы просто из (1)<sub>7</sub> вычитаем (5)<sub>0</sub>

$$x = (1)_7 \ominus (5)_0$$

$$x = [3,2]... \ominus [0,5]...$$

$$x = [3,7]...$$

Находим цикл получивщегося числа и смещение, в результате получаем (1)<sub>47</sub>

$$(1)_7 = (5)_0 + (1)_{47}$$

```
[3,2] -> [9,3] -> [4,9] -> [5,4] -> [9,5]
                    =
[0,5] -> [5,0] -> [5,5] -> [0,5] -> [5,0]
                   (+)
[3,7] -> [4,3] -> [9,4] -> [5,9] -> [4,5]
```

> Можно разложить начало лент, а смещение найти потом и пременить ко всем элементам разложения

В некоторых разрядностях можно разложить Ленту на две других, отличных от первой, например для 5-и значнных чисел

## Блоки

$$
\begin{CD}
  \begin{matrix} 5 & 3 \\
                 4 & 2     \end{matrix}
  @>\text{code}>>
  \begin{matrix} 8 & 5 \\
                 8 & 4     \end{matrix}
  \\
  @VcodeVV 
  \begin{matrix} - & - \\
                 - & -     \end{matrix}
  @VVcodeV 
  \\
  \begin{matrix} 9 & 9 \\
                 5 & 3     \end{matrix}
  @>\text{code}>>
  \begin{matrix} 0 & 9 \\
                 8 & 5     \end{matrix}\end{CD}
$$

## 2-ух значные коды

***Правила:***

$$A = X_A\oplus X^2_A$$

**Влияние на все цифры:**
- A<sub>1</sub>: Через 1 кодирование
- A<sub>2</sub>: Через 2 кодирования

***Ленты:***
```
 ID.  First: len 
(0).  [0,0]:   1
(1).  [0,1]:  60
(2).  [0,2]:  20
(5).  [0,5]:   3
(13). [1,3]:  12
(26). [2,6]:   4
```

*Суммы лент без смещения:*

|   _|             (26)|             (13)|             (5)|             (2)|            (1)|
|----|-----------------|-----------------|----------------|----------------|---------------|
| (1)| (1)<sub>24</sub>| (1)<sub>52</sub>|(2)<sub>15</sub>|(1)<sub>15</sub>|(2)<sub>0</sub>|
| (2)| (2)<sub>12</sub>| (1)<sub>41</sub>|(1)<sub>45</sub>| (2)<sub>5</sub>|               |
| (5)| (13)<sub>1</sub>| (13)<sub>8</sub>| (0)<sub>0</sub>|                |               |
|(13)| (13)<sub>3</sub>| (26)<sub>0</sub>|                |                |               |
|(26)| (26)<sub>1</sub>|                 |                |                |               |

## 3-х значные коды

**Влияние на все цифры:**
- A<sub>1</sub>: Через 3 кодирования
- A<sub>2</sub>: Через 2 кодирования
- A<sub>3</sub>: Через 4 кодирования

***Ленты:***
```
 ID.   First: len 
(0). [0,0,0]:   1
(1). [0,0,1]: 434
(2). [0,0,2]:  62
(3). [0,0,3]: 434
(4). [0,0,4]:  62
(5). [0,0,5]:   7
```

*Суммы лент без смещения:*

|  _|              (5)|              (4)|             (2)|            (3)|            (1)|
|---|-----------------|-----------------|----------------|---------------|---------------|
|(1)| (4)<sub>31</sub>|  (5)<sub>0</sub>| (4)<sub>0</sub>|(3)<sub>0</sub>|(2)<sub>0</sub>|
|(2)|(3)<sub>217</sub>| (4)<sub>31</sub>| (5)<sub>0</sub>|(4)<sub>0</sub>|               |
|(3)| (2)<sub>31</sub>|(3)<sub>217</sub>|(4)<sub>31</sub>|               |               |
|(4)|(1)<sub>217</sub>| (2)<sub>31</sub>|                |               |               |
|(5)|  (0)<sub>0</sub>|                 |                |               |               |

## 5-и значные коды

**Влияние на все цифры:**

- A<sub>1</sub>: Через 7 кодирования
- A<sub>2</sub>: Через 5 кодирования
- A<sub>3</sub>: Через 4 кодирования
- A<sub>4</sub>: Через 6 кодирования
- A<sub>5</sub>: Через 8 кодирования

***Ленты:***
```
 ID.       First:   len 
(0). [0,0,0,0,0]:     1
(1). [0,0,0,0,1]: 24211
(2). [0,0,0,0,2]:   781
(3). [0,0,0,0,3]: 24211
(4). [0,0,0,0,4]:   781
(5). [0,0,0,0,5]:    31
(6). [0,0,0,0,6]:   781
(7). [0,0,0,0,7]: 24211
(8). [0,0,0,0,8]:   781
(9). [0,0,0,0,9]: 24211
```

*Суммы лент:*
Так как любую цифру в 5 позиции пожно представить суммой без переноса двух других цифр (по 4 варианта без учета перестоновки) то лоюбой цикл можно представить в виде суммы двух других, отличных от изначального. После этого можно применить смещение исходного цикла к смещению элементов суммы

Так-же любую ленту можно разложить через насколько (1)

*Ленты для цифр в позиции:*
```
       1    2    3    4    5
"1"  (1)  (1)  (1)  (9)  (1)
"2"  (2)  (2)  (2)  (8)  (2)
"3"  (3)  (3)  (3)  (7)  (3)
"4"  (4)  (4)  (4)  (6)  (4)
"5"  (5)  (5)  (5)  (5)  (5)
"6"  (6)  (6)  (6)  (4)  (6)
"7"  (7)  (7)  (7)  (3)  (7)
"8"  (8)  (8)  (8)  (2)  (8)
"9"  (9)  (9)  (9)  (1)  (9)
```

## 4-х значные коды

***Ленты:***
```
|   ID. First:     len |     ID. First:     len |     ID. First:     len |
|  (0). [0,0,0,0]:   1 |   (29). [0,0,2,9]: 434 | (1105). [1,1,0,5]:  62 |
|  (1). [0,0,0,1]: 434 |   (30). [0,0,3,0]: 434 | (1107). [1,1,0,7]:  62 |
|  (2). [0,0,0,2]:  62 |   (32). [0,0,3,2]: 434 | (1109). [1,1,0,9]:   2 |
|  (3). [0,0,0,3]: 434 |   (34). [0,0,3,4]: 434 | (1125). [1,1,2,5]:  62 |
|  (4). [0,0,0,4]:  62 |   (36). [0,0,3,6]: 434 | (1141). [1,1,4,1]:  62 |
|  (5). [0,0,0,5]:   7 |   (40). [0,0,4,0]:  62 | (1147). [1,1,4,7]:  62 |
| (10). [0,0,1,0]: 434 |   (42). [0,0,4,2]:  62 | (1149). [1,1,4,9]:  62 |
| (12). [0,0,1,2]: 434 |   (48). [0,0,4,8]:  62 | (1159). [1,1,5,9]:  14 |
| (13). [0,0,1,3]: 434 |   (50). [0,0,5,0]:   7 | (1161). [1,1,6,1]:  62 |
| (14). [0,0,1,4]: 434 |  (124). [0,1,2,4]: 434 | (1169). [1,1,6,9]:  62 |
| (15). [0,0,1,5]: 434 |  (129). [0,1,2,9]: 434 | (2203). [2,2,0,3]:  14 |
| (17). [0,0,1,7]: 434 |  (132). [0,1,3,2]: 434 | (2208). [2,2,0,8]:   2 |
| (18). [0,0,1,8]: 434 |  (137). [0,1,3,7]: 434 | (2258). [2,2,5,8]:  14 |
| (20). [0,0,2,0]:  62 |  (248). [0,2,4,8]:  62 | (3307). [3,3,0,7]:   2 |
| (21). [0,0,2,1]: 434 |  (264). [0,2,6,4]:  62 | (4406). [4,4,0,6]:   2 |
| (24). [0,0,2,4]:  62 | (1101). [1,1,0,1]:  62 | (5505). [5,5,0,5]:   1 |
| (25). [0,0,2,5]: 434 | (1103). [1,1,0,3]:  62 |                        |
| (26). [0,0,2,6]:  62 | (1104). [1,1,0,4]:  14 |                        |
```

## Использование

```haskell
import HomaCode

-- Test
main :: IO ()
main = do
  putStrLn $ code [1,2,3,4]
```
