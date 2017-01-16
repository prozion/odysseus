# **Odysseus**

Odysseus – My Swiss Army knife for data treatment: scrap, store, visualize.

---

## Как запустить утилиты odysseus/scrap/vk?


Например это можно сделать вот так

- скачиваем репозиторий в `c:\bin\odysseus`
- создаём новую переменную окружения `ODYSSEUS=c:\bin\odysseus`
- также добавляем `c:\bin\odysseus\cmd` к переменной окружения `PATH`
- если в системе не установлен язык программирования Racket:
  - [скачиваем Racket](https://racket-lang.org/download)
  - устанавливаем Racket (например в `c:\bin\racket6.6`)
  - добавляем `c:\bin\racket6.6` к переменной окружения `PATH`
- для более удобной работы в windows-консоли (форматированный вывод, copy-paste и др.) рекомендуется использовать [ConEmu](https://conemu.github.io)

После этого утилиты vk должны запускаться из консоли в любой директории.

Чтобы узнать какие ключи и опции доступны, наберите `vk -h`
