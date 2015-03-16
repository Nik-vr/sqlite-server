## Description / Описание ##
Multi-thread server for SQlite library. Can connect to remote DB many clients. It uses TCP-sockets.

Многопоточный сервер для SQLite, обеспечивающий совместный доступ к БД с нескольких удалённых клиентов. Передача данных осуществляется с помощью TCP-сокетов по собственному протоколу.

For compile sqlite-server, you need to add to this folder some components:

Для компиляции необходимо скопировать в подкаталог components исходники следующих компонентов: [lazarus-zlib](http://code.google.com/p/lazarus-zlib/), [lazarus-sqlite](http://petrochenko.ru/lazarus/lazarus-sqlite.html), [synapse](http://www.ararat.cz/synapse/).