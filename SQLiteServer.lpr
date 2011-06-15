Program SQLiteServer;

Uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 CThreads,
 {$ENDIF}{$ENDIF}
 DaemonApp, lazdaemonapp, daemon, server_main;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
