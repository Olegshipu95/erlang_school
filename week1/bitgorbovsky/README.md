# Part 1 - basic
[Original link to homework](https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/1-basic.md)  

# Вопросы по главе ```Работа с Erlang shell```

### Вопросы:
- Внимательно изучите вывод комманды help().
- С помощью какой команды можно посмотреть историю?
- С помощью какой команды можно посмотреть статистику потребления памяти?
- Как узнать текущий uptime запущенной оболочки?
- Как грамотно завершить работу оболочки?

### Ответы:
- ```text
  1> help().
  ** shell internal commands **
  b()        -- display all variable bindings
  e(N)       -- repeat the expression in query <N>
  f()        -- forget all variable bindings
  f(X)       -- forget the binding of variable X
  h()        -- history
  h(Mod)     -- help about module
  h(Mod,Func)-- help about function in module
  h(Mod,Func,Arity) -- help about function with arity in module
  ht(Mod)    -- help about a module's types
  ht(Mod,Type) -- help about type in module
  ht(Mod,Type,Arity) -- help about type with arity in module
  hcb(Mod)    -- help about a module's callbacks
  hcb(Mod,CB) -- help about callback in module
  hcb(Mod,CB,Arity) -- help about callback with arity in module
  history(N) -- set how many previous commands to keep
  results(N) -- set how many previous command results to keep
  catch_exception(B) -- how exceptions are handled
  v(N)       -- use the value of query <N>
  rd(R,D)    -- define a record
  rf()       -- remove all record information
  rf(R)      -- remove record information about R
  rl()       -- display all record information
  rl(R)      -- display record information about R
  ...

- Историю можно посмотреть командой ```h().```
- С помощью команды ```memory().``` можно посмотреть статистику потребления памяти.
- Чтобы узнать текущий uptime запущенной оболочки можно ввести команду ```uptime().```
- Грамотно завершить работу оболочки можно командой ```q().```

# Объяснение главы ```Выражения Erlang, переменные, типы данных.```

### Решение 

#### Условие

Простые выражения. Введите в оболочку Erlang следующие выражения и объясните результат.

- `B = 1.`
- `1 = B.`
- `1 = C.`
- `C = 1.`
- `C = B.`
- `A = F = D = B.` (Что в данном случае происходит с переменными A, F, D при
  наличи уже связанной переменной B?)

#### Вывод кода

```text
1> B=1.
1
2> 1=B.
1
3> 1=C.
* 1:3: variable 'C' is unbound
  4> C=1.
  1
  5> C=B.
  1
  6> A=F=D=B.
  1
```

Pattern matching возвращает значение связанной переменной при успешной присваивании. Следовательно B=1 и C=1 вернет 1. 
1=C. вернет ошибку, потому что переменная C до этого не была связана.  

# Объяснение главы ```Сопоставление с образцом.```

#### Задание
- Объясните, почему выражение `Path = "/bar/foo", Bar ++ "/foo" = Path` не
  может быть вычислено интерпретатором и порождает ошибку?
- А сработает ли следующее выражение: `"/bar/" ++ Foo = Path.`? Что является
  его результатом?
- Каков результат следующего выражения?

#### Решение
- 