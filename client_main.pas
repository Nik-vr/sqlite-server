unit client_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, CurrencyEdit, blcksock, SQLiteTable3, SocketsZlib;

type

  { TForm1 }

  TForm1 = class(TForm)
   FieldEdit: TEdit;
   Label5: TLabel;
   Label6: TLabel;
   SelectBox: TListBox;
   TypeBox: TComboBox;
    CurrencyEdit: TCurrencyEdit;
    Label3: TLabel;
    Label4: TLabel;
    TxtEdit: TEdit;
    IP_Edit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
   LogBox: TListBox;
   SendDataButton: TButton;
   procedure FormClose(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure FormShow(Sender: TObject);
   procedure SendDataButtonClick(Sender: TObject);
  private
   procedure AddToLog(Txt: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  MySock: TTCPBlockSocket;
  MyTable: TSQLiteTable;
  MyBase: TSQLiteDatabase;
  WorkDir: string;
  TmpDBFile: string;

implementation

{$R *.lfm}

procedure TForm1.FormClose(Sender: TObject);
begin
 // освобождение сокета
 MySock.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 WorkDir := ExtractFilePath(Application.ExeName);
 TmpDBFile := WorkDir+'tmpdb.db3';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
 CurrencyEdit.Value := 1;
end;

// Функция выполнения запроса к серверу
procedure TForm1.SendDataButtonClick(Sender: TObject);
var
 sock: TTCPBlockSocket;
 SStream, RStream: TMemoryStream;
 i, j, max: integer;
 PackType: byte = 0;
 ErrMsg: string = '';
 Respond: string = '';
begin
  sock := ttcpblocksocket.Create;
  SStream := TMemoryStream.Create;
  RStream := TMemoryStream.Create;

  if sock.LastError <> 0 then AddToLog(sock.LastErrorDesc);
  try
   sock.Family := SF_IP4; // установка типа используемого протокола
   if sock.LastError <> 0 then AddToLog(sock.LastErrorDesc);
   sock.SetSendTimeout(100);
   sock.SetRecvTimeout(100);

   max := round(CurrencyEdit.Value);
   ProgressBar1.Max := max;
   ProgressBar1.Position := 0;

   // установление соединения с сервером
   sock.Connect(IP_Edit.Text,'5150');
   if sock.LastError <> 0 then AddToLog(sock.LastErrorDesc);

   // отправка заданного количества запросов
   for i := 1 to max do
    begin
     ProgressBar1.Position := i;

     // формирование пакета и отправка его серверу
     SStream := CreateSocketString(TxtEdit.Text,TypeBox.ItemIndex+1);
     SStream.Seek(0, soBeginning);
     sock.SendStream(SStream);
     if sock.LastError <> 0
      then AddToLog(sock.GetErrorDescEx+IntTostr(sock.LastError))
      else AddToLog('Отправлен запрос размером '+IntToStr(SStream.Size)+' байт');

     // ожидание ответа сервера
     while true do
     begin
     if sock.WaitingData > 0 then
      begin
       // получение ответа сервера
       RStream := TMemoryStream.Create;
       sock.RecvStream(RStream,100);
       if sock.LastError <> 0
        then AddToLog(sock.GetErrorDescEx)
        else AddToLog('Получен ответ сервера ('+IntToStr(RStream.Size)+' байт)');

       if RStream.Size > 0 then
        begin
         // Разбор ответа
         if ExtractSocketStream(RStream, TmpDBFile, PackType, Respond, ErrMsg) then
         case PackType of
         ptBase:
           begin

            // подключение БД
            MyBase := TSQLiteDatabase.Create(TmpDBFile);
            MyTable := MyBase.GetTable('SELECT * FROM myres');
            // Отключение БД и удаление временного файла
            MyBase.Free;
            DeleteFile(TmpDBFile);

            if MyTable.Count>0 then
             begin
              SelectBox.Clear;
              for j:=0 to MyTable.Count-1 do
               begin
                SelectBox.Items.Add(MyTable.FieldAsString(FieldEdit.Text));
                MyTable.Next;
               end;
             end;
           end;
         ptInfo: AddToLog(Respond);
         end;
        end;
       break;
      end;
     sleep(100);
     end; // wile
    end;
    sock.CloseSocket;
  finally
    sock.Free;
    SStream.Free;
  end;
end;

// Функция ведения logа
procedure TForm1.AddToLog(Txt: string);
begin
 LogBox.Items.Add('['+TimeToStr(Time)+'] '+Txt);
 Application.ProcessMessages;
end;

end.

