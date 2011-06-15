unit uLog;

interface

procedure sLog(aLogFileName: string; aMessage: string) ;

implementation

uses SysUtils, DateUtils;

procedure sLog (aLogFileName: String; aMessage: string);
var
 LF: Text;
 S: String;
begin
 S:='';
 AssignFile(LF,aLogFileName);
 if FileExists(aLogFileName)
  then append(LF)
  else rewrite(LF);

 S:='['+TimeToStr(Time)+'] '+aMessage;

 writeln(LF,S);
 closeFile(LF);
end;

end.
