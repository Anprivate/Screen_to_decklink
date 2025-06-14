unit Unit_main_screen_capture;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Winapi.DirectShow9,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  System.DateUtils, System.IOUtils, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Menus,
  IdGlobalProtocols,
  {
    ffmpeg_types,
    libavcodec,
    libavdevice,
    libavfilter,
    libavformat,
    libavutil,
    libpostproc,
    libswresample,
    libswscale,}
  DeckLinkAPI, DeckLinkAPI.Discovery, DeckLinkAPI.Modes,
  unit_capturer_thread,
  Unit_scaler_thread, Unit_select, Unit_window_select;

type
  TDeckLinkCardObject = class
  private
    fName: string;
    fCard: IDecklink;
  public
    property Name: string read fName;
    property Card: IDecklink read fCard;
    constructor Create(const name: string; const Card: IDecklink);
  end;

  TDeckLinkdisplayModeObject = class
  private
    fName: string;
    fvideoDisplayMode: IDeckLinkdisplayMode;
  public
    property Name: string read fName;
    property videoDisplayMode: IDeckLinkdisplayMode read fvideoDisplayMode;
    constructor Create(const name: string;
      const videoDisplayMode: IDeckLinkdisplayMode);
  end;

  TFormScreenCapture = class(TForm, IDeckLinkVideoOutputCallback)
    Timer1: TTimer;
    MemoMain: TMemo;
    ComboBoxDecklinkCard: TComboBox;
    ComboBoxDecklinkMode: TComboBox;
    ButtonStartStop: TButton;
    PanelState: TPanel;
    LabelCaptionD: TLabel;
    LabelScalerD: TLabel;
    LabelDecklinkD: TLabel;
    LabelCapture: TLabel;
    LabelScaler: TLabel;
    LabelDecklink: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelFPlayed: TLabel;
    LabelFRepeated: TLabel;
    LabelCaptureFPS: TLabel;
    ComboBoxFlicker: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    ComboBoxWhatCapture: TComboBox;
    ButtonSelect: TButton;
    LabelWhatCapture: TLabel;
    ComboBoxAR: TComboBox;
    //
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBoxDecklinkCardChange(Sender: TObject);
    procedure ComboBoxDecklinkModeChange(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxFlickerChange(Sender: TObject);
    procedure ComboBoxWhatCaptureChange(Sender: TObject);
    procedure ButtonSelectClick(Sender: TObject);
    procedure fill_what_capture_label;
    //
    procedure OnWindowMissed(var Message: TMessage); message WM_WINDOW_MISSED;
  private
    ini_capt_buff: Integer;
    ini_scal_buff: Integer;
    ini_decklink_buff: Integer;
    ini_capture_region: TRect;
    ini_window_name: string;
    capture_window_handle: HWND;
    //
    free_rgb_frames: TThreadList<TRGB_frame>;
    filled_rgb_frames: TThreadList<TRGB_frame>;
    yuv_frames: TThreadList<TYuv_frame>;
    //
    Capturer_thread: TCapturer_thread;
    Scaler_thread: TScaler_thread;
    running_state: boolean;
    //
    selected_DeckLink: IDecklink;
    selected_DeckLink_name: string;
    //
    Decklink_output: IDecklinkOutput;
    //
    selected_Decklink_mode: IDeckLinkdisplayMode;
    selected_Decklink_mode_name: string;
    //
    totalFramesScheduled: int64;
    normal_frames, repeated_frames: int64;
    current_timeScale: int64;
    fFPS: Real;
    tmp_frame_buffer: Pointer;
    //
    procedure StartRunning;
    procedure StopRunning;
    //
    procedure Decklink_init;
    procedure RefreshDisplayModeMenu();
    function ScheduleNextFrame(prerolling: boolean): HRESULT;
    //
    procedure WriteToLog(chto: string);
  public
    { Public declarations }
  protected
    function ScheduledFrameCompleted(const completedFrame: IDeckLinkVideoFrame;
      results: _BMDOutputFrameCompletionResult): HRESULT; stdcall;
    function ScheduledPlaybackHasStopped: HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  EMyOwnException = class(Exception);

var
  FormScreenCapture: TFormScreenCapture;

implementation

{$R *.dfm}
function enumListOfTasks(hWindow: HWND; param: lParam): Bool; stdcall; forward;

{ TDeckLinkCardObject }
constructor TDeckLinkCardObject.Create(const name: string;
  const Card: IDecklink);
begin
  fName := name;
  fCard := Card;
end;

{ TDeckLinkdisplayModeObject }
constructor TDeckLinkdisplayModeObject.Create(const name: string;
  const videoDisplayMode: IDeckLinkdisplayMode);
begin
  fName := name;
  fvideoDisplayMode := videoDisplayMode;
end;

{ TFormScreenCapture }
procedure TFormScreenCapture.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  BuildTime: TDateTime;
  nScreenWidth, nScreenHeight: Integer;
  tmp_frame: TRGB_frame;
  i: Integer;
  hr: HRESULT;
begin
  MemoMain.Clear;

  WriteToLog('Программа запущена');

  BuildTime := TFile.GetLastWriteTime(ParamStr(0));
  Self.Caption := 'Захват экрана и выдача на Decklink (build ' +
    formatdatetime('yyyy-mm-dd hh:nn:ss', BuildTime) + ')';

  hr := CoInitialize(nil);
  if not SUCCEEDED(hr) then
    WriteToLog(SysErrorMessage(GetLastError()));

  // ini params read
  ini := TIniFile.Create(extractfilepath(ParamStr(0)) + 'settings.ini');
  try
    Self.Width := ini.ReadInteger('common', 'width', 500);
    Self.Height := ini.ReadInteger('common', 'height', 500);
    Self.Left := ini.ReadInteger('common', 'left', 0);
    Self.Top := ini.ReadInteger('common', 'top', 0);

    ini_capt_buff := ini.ReadInteger('buffers', 'capturer', 5);
    ini_scal_buff := ini.ReadInteger('buffers', 'scaler', 5);
    ini_decklink_buff := ini.ReadInteger('buffers', 'decklink', 5);

    ComboBoxFlicker.ItemIndex := ini.ReadInteger('filter', 'step', 0);

    ini_capture_region.Left := ini.ReadInteger('capture', 'left', 0);
    ini_capture_region.Top := ini.ReadInteger('capture', 'top', 0);
    ini_capture_region.Width := ini.ReadInteger('capture', 'width', 500);
    ini_capture_region.Height := ini.ReadInteger('capture', 'height', 500);

    ini_window_name := ini.ReadString('capture', 'window_name', '');

    ComboBoxWhatCapture.ItemIndex := ini.ReadInteger('capture', 'what', 0);
    ComboBoxWhatCaptureChange(Self);

    ComboBoxAR.ItemIndex := ini.ReadInteger('capture', 'AR', 0);

    if ini_capt_buff < 1 then
    begin
      WriteToLog('Screen capturer''s buffer - too small');
      ini_capt_buff := 5;
    end;

    if ini_scal_buff < 1 then
    begin
      WriteToLog('Scaler''s buffer - too small');
      ini_scal_buff := 5;
    end;

    if ini_decklink_buff < 1 then
    begin
      WriteToLog('Decklink''s buffer - too small');
      ini_decklink_buff := 5;
    end;

    selected_DeckLink_name := ini.ReadString('decklink', 'name', '');
    selected_Decklink_mode_name := ini.ReadString('decklink', 'mode', '');
  finally
    ini.Free;
  end;

  selected_DeckLink := nil;
  selected_Decklink_mode := nil;

  Decklink_init;

  Scaler_thread := nil;
  Capturer_thread := nil;

  // rgb_frames creation
  free_rgb_frames := TThreadList<TRGB_frame>.Create;
  free_rgb_frames.Duplicates := dupAccept;

  filled_rgb_frames := TThreadList<TRGB_frame>.Create;
  filled_rgb_frames.Duplicates := dupAccept;

  nScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  nScreenHeight := GetSystemMetrics(SM_CYSCREEN);

  for i := 0 to ini_capt_buff - 1 do
  begin
    tmp_frame := TRGB_frame.Create(nScreenWidth, nScreenHeight);
    if not Assigned(tmp_frame.data) then
    begin
      WriteToLog('RGB frames can''t be created - out of memory');
      break;
    end;

    free_rgb_frames.Add(tmp_frame);
  end;
end;

procedure TFormScreenCapture.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  ini: TIniFile;
  tmp_list: TList<Unit_capturer_thread.TRGB_frame>;
  i: Integer;
begin
  StopRunning;

  // ini params read
  ini := TIniFile.Create(extractfilepath(ParamStr(0)) + 'settings.ini');
  try
    ini.WriteString('decklink', 'name', selected_DeckLink_name);
    ini.WriteString('decklink', 'mode', selected_Decklink_mode_name);

    ini.WriteInteger('common', 'width', Self.Width);
    ini.WriteInteger('common', 'height', Self.Height);
    ini.WriteInteger('common', 'left', Self.Left);
    ini.WriteInteger('common', 'top', Self.Top);

    ini.WriteInteger('capture', 'what', ComboBoxWhatCapture.ItemIndex);
    ini.WriteInteger('capture', 'left', ini_capture_region.Left);
    ini.WriteInteger('capture', 'top', ini_capture_region.Top);
    ini.WriteInteger('capture', 'width', ini_capture_region.Width);
    ini.WriteInteger('capture', 'height', ini_capture_region.Height);
    ini.WriteString('capture', 'window_name', ini_window_name);
    ini.WriteInteger('capture', 'AR', ComboBoxAR.ItemIndex);

    ini.WriteInteger('filter', 'step', ComboBoxFlicker.ItemIndex);
  finally
    ini.Free;
  end;

  if Assigned(free_rgb_frames) then
  begin
    tmp_list := free_rgb_frames.LockList;
    for i := 0 to tmp_list.Count - 1 do
      if Assigned(tmp_list.Items[i]) then
        TRGB_frame(tmp_list.Items[i]).Free;
    tmp_list.Clear;
    free_rgb_frames.UnlockList;
    free_rgb_frames.Free;
    free_rgb_frames := nil;
  end;

  if Assigned(filled_rgb_frames) then
  begin
    tmp_list := filled_rgb_frames.LockList;
    for i := 0 to tmp_list.Count - 1 do
      if Assigned(tmp_list.Items[i]) then
        TRGB_frame(tmp_list.Items[i]).Free;
    tmp_list.Clear;
    filled_rgb_frames.UnlockList;
    filled_rgb_frames.Free;
    filled_rgb_frames := nil;
  end;

  CoUninitialize();
end;

procedure TFormScreenCapture.ButtonSelectClick(Sender: TObject);
var
  ScreenForm: TFormSelect;
begin
  case ComboBoxWhatCapture.ItemIndex of
    1: // region
      begin
        // создаем нашу полупрозрачную форму
        ScreenForm := TFormSelect.Create(Self);
        try
          // и растягиваем её на весь экран
          ScreenForm.Width := Screen.DesktopWidth;
          ScreenForm.Height := Screen.DesktopHeight;
          ScreenForm.Left := 0;
          ScreenForm.Top := 0;

          ScreenForm.SetInitialRect(ini_capture_region.Left,
            ini_capture_region.Top, ini_capture_region.Right,
            ini_capture_region.Bottom);

          ScreenForm.ARmode := ComboBoxAR.ItemIndex;

          // дальше прячем основную форму
          Self.Hide;

          // показываем полупрозрачную заливку
          ScreenForm.ShowModal;

          Self.Show;

          ini_capture_region := ScreenForm.Selection;
        finally
          ScreenForm.Free;
        end;
      end;
    2 .. 3: // window (full or client)
      if FormWindowSelect.ShowModal = mrOK then
      begin
        ini_window_name := FormWindowSelect.WindowName;
        Winapi.Windows.PostMessage(Self.Handle, WM_WINDOW_MISSED, 0, 0);
      end;
  end;

  fill_what_capture_label();
end;

procedure TFormScreenCapture.ButtonStartStopClick(Sender: TObject);
begin
  if running_state then
    StopRunning
  else
    StartRunning;
end;

procedure TFormScreenCapture.ComboBoxDecklinkCardChange(Sender: TObject);
var
  tmp_DeckLinkCardObject: TDeckLinkCardObject;
  idx: Integer;
begin
  idx := ComboBoxDecklinkCard.ItemIndex;
  tmp_DeckLinkCardObject := TDeckLinkCardObject
    (ComboBoxDecklinkCard.Items.Objects[idx]);

  selected_DeckLink := tmp_DeckLinkCardObject.Card;
  selected_DeckLink_name := tmp_DeckLinkCardObject.name;

  // Obtain the audio/video output interface (IDeckLinkOutput)
  if FAILED(selected_DeckLink.QueryInterface(IID_IDeckLinkOutput,
    Decklink_output)) then
  begin
    WriteToLog('Can''t obtain output interface for card ' +
      selected_DeckLink_name);
    ComboBoxDecklinkMode.Enabled := false;
    Decklink_output := nil;
    Exit;
  end;

  RefreshDisplayModeMenu;
end;

procedure TFormScreenCapture.ComboBoxDecklinkModeChange(Sender: TObject);
var
  VideoFormat: TDeckLinkdisplayModeObject;
  idx: Integer;
begin
  idx := ComboBoxDecklinkMode.ItemIndex;
  VideoFormat := ComboBoxDecklinkMode.Items.Objects[idx]
    as TDeckLinkdisplayModeObject;
  selected_Decklink_mode := VideoFormat.videoDisplayMode;
  selected_Decklink_mode_name := VideoFormat.name;
end;

procedure TFormScreenCapture.ComboBoxFlickerChange(Sender: TObject);
begin
  if running_state and Assigned(Scaler_thread) then
    Scaler_thread.filter := ComboBoxFlicker.ItemIndex;
end;

procedure TFormScreenCapture.ComboBoxWhatCaptureChange(Sender: TObject);
begin
  case ComboBoxWhatCapture.ItemIndex of
    0: // full screen
      begin
        ButtonSelect.Caption := 'Select';
        ButtonSelect.Enabled := false;
        LabelWhatCapture.Caption := '';
        ComboBoxAR.Visible := false;
      end;
    1: // screen region
      begin
        ButtonSelect.Caption := 'Select region';
        ButtonSelect.Enabled := true;
        ComboBoxAR.Visible := true;
      end;
    2 .. 3: // selected application
      begin
        ButtonSelect.Caption := 'Select window';
        ButtonSelect.Enabled := true;
        ComboBoxAR.Visible := false;
      end;
  end;

  fill_what_capture_label();

  if running_state and Assigned(Capturer_thread) then
  begin
    Capturer_thread.capture_mode := ComboBoxWhatCapture.ItemIndex;
    Capturer_thread.capture_rect := ini_capture_region;
    Capturer_thread.capture_window := capture_window_handle;
  end;
end;

// Populate the display mode combo with a list of display modes supported by the installed DeckLink card
procedure TFormScreenCapture.RefreshDisplayModeMenu;
var
  displayModeIterator: IDeckLinkDisplayModeIterator;
  deckLinkDisplayMode: IDeckLinkdisplayMode;

  i: Integer;
  modeName: WideString;
  VideoFormat: TDeckLinkdisplayModeObject;
  idx: Integer;
begin
  idx := 0;
  selected_Decklink_mode := nil;

  ComboBoxDecklinkMode.Enabled := false;

  // clean all associated objects
  for i := 0 to ComboBoxDecklinkMode.Items.Count - 1 do
  begin
    VideoFormat := ComboBoxDecklinkMode.Items.Objects[i]
      as TDeckLinkdisplayModeObject;
    VideoFormat.Free;
  end;

  ComboBoxDecklinkMode.Clear;

  if not Assigned(Decklink_output) then
    Exit;

  try
    if FAILED(Decklink_output.GetDisplayModeIterator(displayModeIterator)) then
      raise EMyOwnException.Create('Can''t enumerate output modes');

    while displayModeIterator.Next(deckLinkDisplayMode) = S_OK do
    begin
      if FAILED(deckLinkDisplayMode.GetName(modeName)) then
        Continue;

      ComboBoxDecklinkMode.Items.AddObject(modeName,
        TDeckLinkdisplayModeObject.Create(modeName, deckLinkDisplayMode));

      if SameText(selected_Decklink_mode_name, modeName) then
      begin
        selected_Decklink_mode := deckLinkDisplayMode;
        idx := ComboBoxDecklinkMode.Items.Count - 1;
      end;
    end;

    if Assigned(displayModeIterator) then
      displayModeIterator := nil;

    if not Assigned(selected_Decklink_mode) and
      (ComboBoxDecklinkMode.Items.Count > 0) then
    begin
      VideoFormat := ComboBoxDecklinkMode.Items.Objects[0]
        as TDeckLinkdisplayModeObject;
      selected_Decklink_mode := VideoFormat.videoDisplayMode;
      selected_Decklink_mode_name := VideoFormat.name;
      idx := 0;
    end;

    ComboBoxDecklinkMode.ItemIndex := idx;
    ComboBoxDecklinkMode.Enabled := true;

    ButtonStartStop.Enabled := Assigned(selected_Decklink_mode);
  except
    on E: EMyOwnException do
    begin
      WriteToLog(E.Message);
    end;
  end;
end;

procedure TFormScreenCapture.Decklink_init;
var
  DeckLinkIterator: IDeckLinkIterator;
  tmp_Decklink: IDecklink;
  tmp_Decklink_name: WideString;
  tmp_Decklink_attributes: IDeckLinkAttributes;
  tmp_VideoIOSupport: int64;
  tmp_idx: Integer;
  tmp_DeckLinkCardObject: TDeckLinkCardObject;

  hr: HRESULT;
begin
  ComboBoxDecklinkCard.Clear;

  selected_DeckLink := nil;
  tmp_idx := 0;

  try
    hr := CoCreateInstance(CLASS_CDeckLinkIterator, nil, CLSCTX_ALL,
      IID_IDeckLinkIterator, DeckLinkIterator);
    if not SUCCEEDED(hr) then
      raise EMyOwnException.Create('No Decklnik card installed ' +
        SysErrorMessage(GetLastError()));

    // iterate Decklink devices
    while DeckLinkIterator.Next(tmp_Decklink) = S_OK do
    begin
      if not Assigned(tmp_Decklink) then
        Continue;

      if FAILED(tmp_Decklink.GetDisplayName(tmp_Decklink_name)) then
        Continue;

      if FAILED(tmp_Decklink.QueryInterface(IID_IDeckLinkAttributes,
        tmp_Decklink_attributes)) then
        Continue;

      if tmp_Decklink_attributes.GetInt(BMDDeckLinkVideoIOSupport,
        tmp_VideoIOSupport) <> S_OK then
        Continue;

      if (tmp_VideoIOSupport and bmdDeviceSupportsPlayback) = 0 then
        Continue;

      tmp_DeckLinkCardObject := TDeckLinkCardObject.Create(tmp_Decklink_name,
        tmp_Decklink);
      ComboBoxDecklinkCard.AddItem(tmp_Decklink_name, tmp_DeckLinkCardObject);

      if SameText(tmp_Decklink_name, selected_DeckLink_name) then
      begin
        selected_DeckLink := tmp_Decklink;
        tmp_idx := ComboBoxDecklinkCard.Items.Count - 1;
      end;
    end;

    ComboBoxDecklinkCard.ItemIndex := tmp_idx;
    if not Assigned(selected_DeckLink) and (ComboBoxDecklinkCard.Items.Count > 0)
    then
    begin
      tmp_DeckLinkCardObject := TDeckLinkCardObject
        (ComboBoxDecklinkCard.Items.Objects[0]);
      selected_DeckLink := tmp_DeckLinkCardObject.fCard;
      selected_DeckLink_name := ComboBoxDecklinkCard.Items.Strings[0];
    end;

    if ComboBoxDecklinkCard.Items.Count > 0 then
    begin
      ComboBoxDecklinkCard.Enabled := true;
    end;

    ComboBoxDecklinkCardChange(Self);
  except
    on E: EMyOwnException do
    begin
      WriteToLog(E.Message);
    end;
  end;
end;

procedure TFormScreenCapture.fill_what_capture_label;
begin
  case ComboBoxWhatCapture.ItemIndex of
    0: // full screen
      LabelWhatCapture.Caption := '';
    1: // screen region
      LabelWhatCapture.Caption := format('Screen region: %ux%u at [%u,%u]',
        [ini_capture_region.Width, ini_capture_region.Height,
        ini_capture_region.Left, ini_capture_region.Top]);
    2 .. 3: // selected application
      LabelWhatCapture.Caption := 'Window name: ' + ini_window_name;
  end;
end;

procedure TFormScreenCapture.FormResize(Sender: TObject);
begin
  MemoMain.Width := Self.ClientWidth - 2 * MemoMain.Left;
  MemoMain.Height := Self.ClientHeight - MemoMain.Top - MemoMain.Left;
  PanelState.Width := MemoMain.Width - (PanelState.Left - MemoMain.Left);
end;

procedure TFormScreenCapture.OnWindowMissed(var Message: TMessage);
begin
  if Length(ini_window_name) > 3 then
  begin
    capture_window_handle := 0;

    enumWindows(@enumListOfTasks, 0);

    if IsWindow(capture_window_handle) and running_state and
      Assigned(Capturer_thread) then
      Capturer_thread.capture_window := capture_window_handle;
  end;
end;

procedure TFormScreenCapture.StartRunning;
var
  frameDuration, timeScale: int64;
  buffer_filling_start: TDateTime;
  hr: HRESULT;
begin
  try
    if not Assigned(selected_DeckLink) then
      raise EMyOwnException.Create('Decklink card not selected');

    if not Assigned(selected_Decklink_mode) then
      raise EMyOwnException.Create('Decklink card mode not selected');

    if not Assigned(Decklink_output) then
      raise EMyOwnException.Create('Decklink card output not active');

    // frames lists preparing
    yuv_frames := TThreadList<TYUV_frame>.Create;
    yuv_frames.Duplicates := dupAccept;

    // start screen capturer thread
    Capturer_thread := TCapturer_thread.Create(true);

    Capturer_thread.free_frames := free_rgb_frames;
    Capturer_thread.filled_frames := filled_rgb_frames;
    Capturer_thread.max_frames_in_list := ini_capt_buff;
    Capturer_thread.capture_rect := ini_capture_region;
    Capturer_thread.capture_mode := ComboBoxWhatCapture.ItemIndex;
    Capturer_thread.mothers_window := Self.Handle;

    Capturer_thread.FreeOnTerminate := false;

    Capturer_thread.Start;

    // start scaler thread
    Scaler_thread := TScaler_thread.Create(true);

    Scaler_thread.free_rgb_frames := free_rgb_frames;
    Scaler_thread.filled_rgb_frames := filled_rgb_frames;
    Scaler_thread.out_frames := yuv_frames;
    Scaler_thread.max_frames_in_list := ini_scal_buff;
    Scaler_thread.Width := selected_Decklink_mode.GetWidth;
    Scaler_thread.Height := selected_Decklink_mode.GetHeight;
    Scaler_thread.filter := ComboBoxFlicker.ItemIndex;
    Scaler_thread.FreeOnTerminate := false;

    Scaler_thread.Start;

    // enable Decklink output
    hr := Decklink_output.EnableVideoOutput
      (selected_Decklink_mode.GetDisplayMode, bmdVideoOutputFlagDefault);
    if FAILED(hr) then
      raise EMyOwnException.Create('Can''t enable video output');

    // Provide this class as a delegate to the audio and video output interfaces
    hr := Decklink_output.SetScheduledFrameCompletionCallback
      (FormScreenCapture);
    if FAILED(hr) then
      raise EMyOwnException.Create('Can''t set callback');

    // prerolling
    totalFramesScheduled := 0;
    normal_frames := 0;
    repeated_frames := 0;

    tmp_frame_buffer := GetMemory(selected_Decklink_mode.GetWidth *
      selected_Decklink_mode.GetHeight * 2);

    buffer_filling_start := Now();
    while totalFramesScheduled < ini_decklink_buff do
    begin
      ScheduleNextFrame(true);

      if MillisecondsBetween(buffer_filling_start, Now()) > 2000 then
        raise EMyOwnException.Create
          ('Input buffers not filled in 2 sec - something wrong');

      Application.ProcessMessages;
      Sleep(5);
    end;

    WriteToLog('Buffers filled');

    hr := selected_Decklink_mode.GetFrameRate(frameDuration, timeScale);
    if FAILED(hr) then
      raise EMyOwnException.Create('Can''t get frame rate for selected mode');

    current_timeScale := timeScale;
    fFPS := timeScale / frameDuration;

    // start playback
    hr := Decklink_output.StartScheduledPlayback(0, timeScale, 1.0);
    if FAILED(hr) then
      raise EMyOwnException.Create('Can''t start');

    WriteToLog('Started');
    running_state := true;

    ComboBoxDecklinkCard.Enabled := false;
    ComboBoxDecklinkMode.Enabled := false;

    ButtonStartStop.Caption := 'Stop';
  except
    on E: EMyOwnException do
    begin
      WriteToLog(E.Message);
      StopRunning;
    end;
  end;
end;

procedure TFormScreenCapture.StopRunning;
var
  tmp_list: Tlist<TYUV_frame>;
  i: Integer;
  actualTime: int64;
  is_active: Integer;
  hr: HRESULT;
begin
  if Assigned(Decklink_output) then
  begin
    hr := Decklink_output.IsScheduledPlaybackRunning(is_active);
    if SUCCEEDED(hr) and (is_active <> 0) then
    begin
      Decklink_output.StopScheduledPlayback(0, actualTime, current_timeScale);
      Decklink_output.DisableVideoOutput;

      WriteToLog('Decklink output stopped');
    end;
  end;

  if Assigned(Scaler_thread) then
  begin
    Scaler_thread.Terminate;

    while not Scaler_thread.finished do
    begin
      Sleep(10);
      Application.ProcessMessages;
    end;
    Scaler_thread.Free;
    Scaler_thread := nil;
  end;

  if Assigned(Capturer_thread) then
  begin
    Capturer_thread.Terminate;
    while not Capturer_thread.finished do
    begin
      Sleep(10);
      Application.ProcessMessages;
    end;
    Capturer_thread.Free;
    Capturer_thread := nil;
  end;

  if Assigned(yuv_frames) then
  begin
    tmp_list := yuv_frames.LockList;
    for i := 0 to tmp_list.Count - 1 do
      if Assigned(tmp_list.Items[i]) then
        TYuv_frame(tmp_list.Items[i]).Free;
    tmp_list.Clear;
    yuv_frames.UnlockList;
    yuv_frames.Free;
    yuv_frames := nil;
  end;

  if Assigned(tmp_frame_buffer) then
  begin
    FreeMemory(tmp_frame_buffer);
    tmp_frame_buffer := nil;
  end;

  ButtonStartStop.Caption := 'Start';

  ComboBoxDecklinkCard.Enabled := true;
  ComboBoxDecklinkMode.Enabled := true;
  // ComboBoxFlicker.Enabled := true;

  running_state := false;
end;

function TFormScreenCapture.ScheduledFrameCompleted(const completedFrame
  : IDeckLinkVideoFrame; results: _BMDOutputFrameCompletionResult): HRESULT;
begin
  result := ScheduleNextFrame(false);
end;

function TFormScreenCapture.ScheduledPlaybackHasStopped: HRESULT;
begin
  result := S_OK;
end;

function TFormScreenCapture.ScheduleNextFrame(prerolling: boolean): HRESULT;
var
  hr: HRESULT;
  Width, Height: Integer;
  theFrame: IDeckLinkMutableVideoFrame;
  frameDuration, timeScale: int64;
  FrameData: Pointer;
  Yuv_frame: TYuv_frame;

  local_list: Tlist<TYUV_frame>;

begin
  result := S_FALSE;

  if not prerolling and not running_state then
    Exit;

  Yuv_frame := nil;

  local_list := yuv_frames.LockList;
  if local_list.Count > 0 then
  begin
    Yuv_frame := local_list.Items[0];
    local_list.Delete(0);
  end;
  yuv_frames.UnlockList;

  if not Assigned(Yuv_frame) and prerolling then
    Exit;

  Width := selected_Decklink_mode.GetWidth;
  Height := selected_Decklink_mode.GetHeight;
  selected_Decklink_mode.GetFrameRate(frameDuration, timeScale);

  hr := Decklink_output.CreateVideoFrame(Width, Height, Width * 2,
    bmdFormat8BitYUV, bmdFrameFlagDefault, theFrame);
  if FAILED(hr) then
    Exit;
  //
  hr := theFrame.GetBytes(FrameData);
  if FAILED(hr) then
    Exit;

  if Assigned(Yuv_frame) then
  begin
    CopyMemory(FrameData, Yuv_frame.data, Yuv_frame.data_size);
    CopyMemory(tmp_frame_buffer, Yuv_frame.data, Yuv_frame.data_size);
    inc(normal_frames);
  end
  else
  begin
    CopyMemory(FrameData, tmp_frame_buffer, Width * Height * 2);
    inc(repeated_frames);
  end;

  hr := Decklink_output.ScheduleVideoFrame(theFrame,
    (totalFramesScheduled * frameDuration), frameDuration, timeScale);
  if FAILED(hr) then
    Exit;

  inc(totalFramesScheduled);

  if Assigned(Yuv_frame) then
    Yuv_frame.Free;
  // theFrame._Release;

  result := S_OK;
end;

procedure TFormScreenCapture.Timer1Timer(Sender: TObject);
var
  local_rgb_list: Tlist<TRGB_frame>;
  local_yuv_list: Tlist<TYUV_frame>;
  capt, scal, dl: Integer;
  dl1: Cardinal;
  captFPS: Real;
begin
  capt := 0;
  scal := 0;
  dl := 0;

  if Assigned(filled_rgb_frames) then
  begin
    local_rgb_list := filled_rgb_frames.LockList;
    capt := local_rgb_list.Count;
    filled_rgb_frames.UnlockList;
  end;

  if Assigned(yuv_frames) then
  begin
    local_yuv_list := yuv_frames.LockList;
    scal := local_yuv_list.Count;
    yuv_frames.UnlockList;
  end;

  if Assigned(Decklink_output) then
  begin
    if SUCCEEDED(Decklink_output.GetBufferedVideoFrameCount(dl1)) then
      dl := dl1;
  end;

  LabelCapture.Caption := format('%d/%d', [capt, ini_capt_buff]);
  LabelScaler.Caption := format('%d/%d', [scal, ini_scal_buff]);
  LabelDecklink.Caption := format('%d/%d', [dl, ini_decklink_buff]);

  LabelFPlayed.Caption := format('%d', [totalFramesScheduled]);
  LabelFRepeated.Caption := format('%d', [repeated_frames]);

  if totalFramesScheduled > 0 then
    captFPS := fFPS * normal_frames / totalFramesScheduled
  else
    captFPS := 0;

  LabelCaptureFPS.Caption := format('%f fps', [captFPS]);
end;

procedure TFormScreenCapture.WriteToLog(chto: string);
var
  tmpstr: string;
  LogPath, LFN: string;
  F1: TextFile;
begin
  tmpstr := DateTimeToStr(Now) + ' ' + chto;

  LogPath := extractfilepath(ParamStr(0)) + 'logs\';
  if not TDirectory.Exists(LogPath) then
    try
      TDirectory.CreateDirectory(LogPath);
    except
      LogPath := extractfilepath(ParamStr(0));
    end;

  LFN := LogPath + 'control.log';

  try
    if FileExists(LFN) and (FileSizeByName(LFN) > 1000000) then
      RenameFile(LFN, LogPath + 'control_' + formatdatetime('yyyy-mm-dd', Now())
        + '.log');

    if FileExists(LFN) then
    begin
      AssignFile(F1, LFN);
      Append(F1);
    end
    else
    begin
      AssignFile(F1, LFN);
      Rewrite(F1);
    end;

    Writeln(F1, tmpstr);

    Flush(F1);
    CloseFile(F1);
  except
    on E: Exception do
      MemoMain.Lines.Append('Сбой при записи лога');
  end;

  MemoMain.Lines.Append(tmpstr);

  if MemoMain.Lines.Count > 100 then
  begin
    while MemoMain.Lines.Count > 100 do
      MemoMain.Lines.Delete(0);
    MemoMain.Perform(EM_SCROLL, SB_BOTTOM, 0);
  end;
end;

function TFormScreenCapture._AddRef: Integer;
begin
  result := 1;
end;

function TFormScreenCapture._Release: Integer;
begin
  result := 1;
end;

function enumListOfTasks(hWindow: HWND; param: lParam): Bool; stdcall;
var
  HoldString: PWideChar;
  WinName: string;
  WindowStyle: Longint;
  IsAChild, IsVisible, IsApp: Bool;
begin
  result := true;

  WindowStyle := GetWindowLong(hWindow, GWL_STYLE);

  IsVisible := (WindowStyle and Longint(WS_VISIBLE)) <> 0;
  IsApp := (WindowStyle and Longint(WS_EX_APPWINDOW)) <> 0;

  IsAChild := GetWindowLong(hWindow, GWL_HWNDPARENT) <> 0;

  if IsVisible and IsApp and not IsAChild then
  begin
    GetMem(HoldString, 256);
    if GetWindowText(hWindow, HoldString, 255) > 0 then
    begin
      WinName := StrPas(HoldString);
      if Length(WinName) > 0 then
        if WinName.StartsWith(FormScreenCapture.ini_window_name) then
        begin
          FormScreenCapture.capture_window_handle := hWindow;
          result := false;
        end;
    end;
    FreeMem(HoldString, 256);
  end;
end;

end.
