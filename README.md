# guns-n-fun

[![Build Status](https://travis-ci.org/cmc-haskell-2018/guns-n-fun.svg?branch=master)](https://travis-ci.org/cmc-haskell-2018/guns-n-fun)

Игра в жанре платформер-шутер.

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec guns-n-fun
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```

