unit SocketsZlib;

interface

uses Classes, SysUtils, ZLibEx, md5;

function CreateSocketStream(SourceFile: string): TMemoryStream;
function CreateSocketString(SourceString: string; PackType: byte): TMemoryStream;
function ExtractSocketStream(CompressedStream: TMemoryStream; OutFile: string; var PackType: byte; var Respond: string; var ErrorMsg: string): boolean;

// Типы пакета
const
  ptSelect = 1;
  ptUpdate = 2;
  ptBase = 3;
  ptInfo = 4;

implementation

// Создаём поток, содержащий указанный файл, его контрольную сумму и тип пакета
function CreateSocketStream(SourceFile: string): TMemoryStream;
var
 fs: TFileStream;
 inStream: TMemoryStream;
 keybit: string = '3';
 CRC: TMD5Digest;
begin
 // Создаём потоки
 inStream := TMemoryStream.Create;
 result := TMemoryStream.Create;

 // Создаём файловый поток и переносим его в память
 fs := TFileStream.Create(SourceFile, fmOpenRead);
 inStream.LoadFromStream(fs);
 fs.Free;

 // Пишем в поток контрольную сумму
 CRC:=MD5Buffer(inStream.Memory^, inStream.Size);
 inStream.Seek(inStream.Size, soBeginning);
 inStream.WriteBuffer(CRC,16);

 // Пишем в поток ключ (тип пакета)
 inStream.Seek(inStream.Size, soBeginning);
 inStream.WriteBuffer(Pointer(keybit)^,Length(keybit));

 // Сжимаем фаловый поток и записываем в строковый
 inStream.Position := 0;
 ZCompressStream(inStream, result, zcFastest);

 // Уничтожаем файловый поток
 inStream.Free;
end;

// Создаём поток, содержащий строку, её контрольную сумму и тип пакета
function CreateSocketString(SourceString: string; PackType: byte): TMemoryStream;
var
 inStream: TMemoryStream;
 keybit: string = '';
 CRC: TMD5Digest;
begin
 // Создаём потоки
 inStream := TMemoryStream.Create;
 result := TMemoryStream.Create;
 keybit := IntToStr(PackType);

 // Пишем в поток исходную строку
 inStream.Seek(inStream.Size, soBeginning);
 inStream.WriteBuffer(Pointer(SourceString)^,Length(SourceString));

 // Пишем в поток контрольную сумму
 CRC:=MD5Buffer(inStream.Memory^, inStream.Size);
 inStream.Seek(inStream.Size, soBeginning);
 inStream.WriteBuffer(CRC,16);

 // Пишем в поток ключ (тип пакета)
 inStream.Seek(inStream.Size, soBeginning);
 inStream.WriteBuffer(Pointer(keybit)^,Length(keybit));

 // Сжимаем фаловый поток и записываем в строковый
 inStream.Position := 0;
 ZCompressStream(inStream, result, zcFastest);

 // Уничтожаем файловый поток
 inStream.Free;
end;

// Распаковка файла из потока
// var PackType - возвращает тип пакета
// var ErrorMsg - возвращает сообщение об ошибке в случае неудачи
// result = true - успешная распаковка, false - ошибка (см. ErrorMsg)
function ExtractSocketStream(CompressedStream: TMemoryStream; OutFile: string; var PackType: byte; var Respond: string; var ErrorMsg: string): boolean;
var
 fs: TFileStream;
 DecompressedStream: TMemoryStream;
 keybit: char;
 CRC: TMD5Digest;
begin
 result := false;
 // Создаём временный поток
 DecompressedStream := TMemoryStream.Create;

 // Распаковываем исходный поток в память
 CompressedStream.Position := 0;
 ZDecompressStream(CompressedStream, DecompressedStream);

 // Отрезаем от потока ключевой байт
 DecompressedStream.Seek(DecompressedStream.Size-1, soBeginning);
 DecompressedStream.Read(keybit, 1);
 PackType := StrToInt(keybit);
 DecompressedStream.Size := DecompressedStream.Size-1;

 // Отрезаем от потока контрольную сумму
 DecompressedStream.Seek(DecompressedStream.Size-16, soBeginning);
 DecompressedStream.Read(CRC, 16);
 DecompressedStream.Size := DecompressedStream.Size-16;

 // В зависимости от типа пакета формируем результат
 case PackType of
 1,2,4: // обычная строка
  begin
   SetLength(Respond, DecompressedStream.Size);
   DecompressedStream.Seek(0, soBeginning);
   DecompressedStream.Read(Pointer(Respond)^, DecompressedStream.Size);
   // Проверка контрольной суммы
   if MD5Match(CRC, MD5String(Respond))
    then result := true
    else result := false;
  end;
 3: // файл
  begin
   // Записываем очищенный поток в файл
   fs := TFileStream.Create(OutFile, fmCreate);
   DecompressedStream.SaveToStream(fs);
   fs.Free;
   // Проверка контрольной суммы
   if MD5Match(CRC, MD5Buffer(DecompressedStream.Memory^, DecompressedStream.Size))
    then result := true
    else result := false;
  end;
 end;
end;

end.
