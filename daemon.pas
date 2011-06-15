unit daemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp, server_main;

type
  TSQLiteDaemonMapper = class(TDaemonMapper)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SQLiteDaemonMapper: TSQLiteDaemonMapper;

implementation

procedure RegisterMapper; 
begin
  RegisterDaemonMapper(TSQLiteDaemonMapper)
end; 

{$R *.lfm}


initialization
  RegisterMapper; 
end.

