program Project_screen_capture;



uses
  Vcl.Forms,
  Unit_main_screen_capture in 'Unit_main_screen_capture.pas' {FormScreenCapture},
  ffmpeg_types in 'source\ffmpeg_types.pas',
  libavcodec in 'source\libavcodec.pas',
  libavdevice in 'source\libavdevice.pas',
  libavfilter in 'source\libavfilter.pas',
  libavformat in 'source\libavformat.pas',
  libavutil in 'source\libavutil.pas',
  libpostproc in 'source\libpostproc.pas',
  libswresample in 'source\libswresample.pas',
  libswscale in 'source\libswscale.pas',
  Unit_capturer_thread in 'Unit_capturer_thread.pas',
  Unit_scaler_thread in 'Unit_scaler_thread.pas',
  DeckLinkAPI.Configuration in 'Include\DeckLinkAPI.Configuration.pas',
  DeckLinkAPI.DeckControl in 'Include\DeckLinkAPI.DeckControl.pas',
  DeckLinkAPI.Discovery in 'Include\DeckLinkAPI.Discovery.pas',
  DeckLinkAPI.Modes in 'Include\DeckLinkAPI.Modes.pas',
  DeckLinkAPI in 'Include\DeckLinkAPI.pas',
  DeckLinkAPI.Streaming in 'Include\DeckLinkAPI.Streaming.pas',
  DeckLinkAPI.Types in 'Include\DeckLinkAPI.Types.pas',
  Unit_select in 'Unit_select.pas',
  Unit_window_select in 'Unit_window_select.pas' {FormWindowSelect};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormScreenCapture, FormScreenCapture);
  Application.CreateForm(TFormWindowSelect, FormWindowSelect);
  Application.Run;
end.
