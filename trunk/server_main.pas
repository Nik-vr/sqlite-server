unit server_main;

{$mode objfpc}{$H+}

interface

uses
  Interfaces, Classes, SysUtils, FileUtil, DaemonApp, blcksock, synsock, uLog,
  SQLiteTable3, SocketsZlib;

// Класс, отвечающий за работу сетевой службы
type
  { TSQLiteDaemon }
  TSQLiteDaemon = class(TDaemon)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
  private
    { private declarations }
  public
    procedure AddToLog(msg: string);
    { public declarations }
  end; 

// Класс, отвечающий за обработку данных в отдельных потоках
type
  TSockThread = class(TThread)
  private
    fSock: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(aSock: TSocket);
    destructor Destroy; override;
    procedure GetSQLRes(RStream: TMemoryStream);
  end;

// Класс, отвечающий за ожидание данных
  type
    TMainThread = class(TThread)
    private
      ListenerSocket: TTCPBlockSocket;
    protected
      procedure Execute; override;
    public
      constructor Create;
      destructor Destroy; override;
    end;

var
  SQLiteDaemon: TSQLiteDaemon;
  WorkDir: string;
  LogFile: string;
  TmpDBFile: string;
  MyTable: TSQLiteTable;
  MyBase: TSQLiteDatabase;

implementation

procedure RegisterDaemon; 
begin
  RegisterDaemonClass(TSQLiteDaemon)
end;

// Функция обработки полученного пакета
procedure TSockThread.GetSQLRes(RStream: TMemoryStream);
var
 SStream: TMemoryStream;
 PackType: byte = 0;
 ErrMsg: string = '';
 Respond: string = '';
begin
 SStream := TMemoryStream.Create;
 sLog(LogFile, 'Принято '+IntToStr(RStream.Size)+' байт');

 // Пробуем разобрать поток
 if ExtractSocketStream(RStream, TmpDBFile, PackType, Respond, ErrMsg) then
  // если распаковали успешно
  begin
   // отправляем ответ в зависимости от типа пакета
   case PackType of
   ptSelect:
    begin
     if Pos('SELECT',Respond) = 1 then
      begin
       // Если временный файл уже существует - удаляем его
       if FileExists(TmpDBFile) then DeleteFile(TmpDBFile);
       // Подключаем временную БД и копируем в ней выборку по запросу
       MyBase.ExecSQL('ATTACH "'+TmpDBFile+'" AS xdb');
       MyBase.ExecSQL('CREATE TABLE [xdb].[myres] AS '+Respond);
       MyBase.ExecSQL('DETACH xdb');
       // Создаём поток с пакетом, готовым к отправке
       SStream := CreateSocketStream(TmpDBFile);
       DeleteFile(TmpDBFile);
      end
     else SStream := CreateSocketString('Запрос не соответствует типу Select!',4);
    end;
   ptUpdate:
    begin
     MyBase.ExecSQL(Respond);
     // добавить функцию получения ошибки
     SStream := CreateSocketString('Обновление базы проведено успешно...',4);
    end;
   // case
   else SStream := CreateSocketString('Неизвестный тип пакета!',4);
   end;
  end
 // если при разборе пакета произошла ошибка
 else
  begin
   sLog(LogFile, 'Ошибка при разборе пакета. Контрольная сумма не совпадает!');
   SStream := CreateSocketString('Контрольные суммы запроса не совпадают!',4);
  end;

 // Отправление ответа клиенту
 SStream.Seek(0, soBeginning);
 fSock.SendStream(SStream);

 if fSock.LastError <> 0
  then SQLiteDaemon.AddToLog('Ошибка отправления: '+fSock.GetErrorDescEx+IntTostr(fSock.LastError))
  else SQLiteDaemon.AddToLog('Отправлен пакет размером '+IntToStr(SStream.Size)+' байт');
end;

{ TSockThread }

// Создание класса-потока
constructor TSockThread.Create(aSock: TSocket);
begin
  FreeOnTerminate := True;
  fSock := TTCPBlockSocket.Create;
  // передаём сокет на обработку классу
  fSock.Socket := aSock;

  inherited Create(False);
end;

// Уничтожение класса-потока
destructor TSockThread.Destroy;
begin
  fSock.Free; // очищаем сокет
  inherited;
end;

// Обработка поступивших от сокета данных в потоке
procedure TSockThread.Execute;
var
 RStream: TMemoryStream;
begin
 RStream := TMemoryStream.Create;
 SQLiteDaemon.AddToLog('Установлено соединение с '+fSock.GetRemoteSinIP);

 // Ожидаем данные от клиента
 while true do
 begin
 if fSock.WaitingData > 0 then
  begin
   // Получаем строку от клиента
   fSock.RecvStream(RStream,100);
   RStream.Position := 0;
   if fSock.LastError <> 0 then SQLiteDaemon.AddToLog(fSock.GetErrorDescEx);

   // Если получили - разбираем пакет и выполняем запрос клиента на работу с БД
   if RStream.Size > 0 then GetSQLRes(RStream);
  end;
  sleep(100);
 end;
end;

{ TMainThread }

// Создание класса-потока
constructor TMainThread.Create;
begin
  FreeOnTerminate := True;

  // Создаём "слушающий" сокет
  ListenerSocket := TTCPBlockSocket.Create;
  if ListenerSocket.LastError = 0
   then SQLiteDaemon.AddToLog('Создан "слушающий" сокет')
   else SQLiteDaemon.AddToLog('Ошибка при создании "слушающего" сокета: '+ListenerSocket.GetErrorDescEx);
  inherited Create(False);
end;

// Уничтожение класса-потока
destructor TMainThread.Destroy;
begin
  // освобождаем сокет
  ListenerSocket.Free;
  if ListenerSocket.LastError = 0
   then SQLiteDaemon.AddToLog('Сокет успешно удален')
   else SQLiteDaemon.AddToLog('Ошибка при удалении сокета: '+ListenerSocket.GetErrorDescEx);
  inherited;
end;

// Обработка поступивших от сокета данных в потоке
procedure TMainThread.Execute;
var
 IP: string;
 ClientSock: TSocket;
begin
 // Начинаем "прослушку"
 with ListenerSocket do
  begin
   // инициализация сокета
   CreateSocket;
   if LastError = 0
    then SQLiteDaemon.AddToLog('Сокет успешно инициализирован')
    else SQLiteDaemon.AddToLog('Ошибка при инициализации сокета: '+GetErrorDescEx);
   Family := SF_IP4; // тип соединения (IPv4)
   setLinger(true,100);

   // получение IP-адреса нашей машины и привязка к нему порта
   IP := ResolveName(LocalName);
   bind(IP, '5150');
   if LastError = 0
    then SQLiteDaemon.AddToLog('Начата прослушка на порту 5150')
    else SQLiteDaemon.AddToLog('Ошибка при начале прослушки: '+GetErrorDescEx);

  // "слушаем" порт и при необходимости создаём клиентские сокеты
  listen;

  repeat
   if CanRead(100) then
    begin
     ClientSock := Accept;
     if LastError = 0
      then TSockThread.Create(ClientSock)
      else SQLiteDaemon.AddToLog('Ошибка: '+GetErrorDescEx);
    end;
    sleep(100);
   until false;
  end;
end;

{ TSQLiteDaemon }

// Запись в журнал событий
procedure TSQLiteDaemon.AddToLog(msg: string);
begin
 sLog(LogFile, msg);
end;

procedure TSQLiteDaemon.DataModuleCreate(Sender: TObject);
begin
 // Рабочий каталог и журнал ошибок
 WorkDir := ExtractFilePath(Application.ExeName);
 LogFile := WorkDir+'errors.log';
 TmpDBFile := WorkDir+'tmpdb.db3';
end;

// Запуск демона
procedure TSQLiteDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
 OK := true;
 MyBase := TSQLiteDatabase.Create(WorkDir+'bookkeeper.db3');
 TMainThread.Create;
end;

{$R *.lfm}

initialization
  RegisterDaemon; 
end.
