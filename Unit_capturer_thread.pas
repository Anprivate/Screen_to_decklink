unit Unit_capturer_thread;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils, System.DateUtils,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Forms;

const
  WM_WINDOW_MISSED = WM_USER + 1;

type
  TRGB_frame = class(TObject)
  private
    l_width, l_height: integer;
    l_bpp: integer;
    l_data: PByte;
    l_data_size: integer;
  public
    property width: integer read l_width;
    property height: integer read l_height;
    property data: PByte read l_data;
    property data_size: integer read l_data_size;
    property bpp: integer read l_bpp;
    //
    Constructor Create(in_width, in_height: integer); overload;
    procedure Fill(in_width, in_height: integer; bmp: TBitMap);
    Destructor Destroy; override;
  end;

  TCapturer_thread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    free_frames: TThreadList<TRGB_frame>;
    filled_frames: TThreadList<TRGB_frame>;
    max_frames_in_list: integer;
    capture_mode: integer;
    capture_rect: TRect; // for mode 1
    capture_window: hWnd; // for mode 2
    mothers_window: hWnd; // for message

    finished: boolean;
  end;

implementation

{ TRGB_frame }
Constructor TRGB_frame.Create(in_width, in_height: integer);
begin
  inherited Create;
  //
  l_width := in_width;
  l_height := in_height;
  l_data_size := in_width * in_height * 4;

  try
    l_data := GetMemory(l_data_size);
  except
    l_data := nil;
  end;
end;

procedure TRGB_frame.Fill(in_width, in_height: integer; bmp: TBitMap);
var
  y: integer;
  row_size: integer;
  tmp_ptr: PByte;
begin
  inherited Create;

  l_width := in_width;
  l_height := in_height;

  case bmp.PixelFormat of
    pf8bit:
      l_bpp := 1;
    pf16bit:
      l_bpp := 2;
    pf24bit:
      l_bpp := 3;
    pf32bit:
      l_bpp := 4;
  end;

  row_size := l_width * l_bpp;
  l_data_size := row_size * l_height;

  tmp_ptr := l_data;
  for y := 0 to l_height - 1 do
  begin
    CopyMemory(tmp_ptr, bmp.ScanLine[y], row_size);
    inc(tmp_ptr, row_size);
  end;
end;

destructor TRGB_frame.Destroy;
begin
  if Assigned(data) then
    FreeMemory(data);

  inherited;
end;

{ capturer_thread }
procedure TCapturer_thread.Execute;
var
  local_list: TList<TRGB_frame>;
  bmp: TBitMap;
  tmp_frame: TRGB_frame;
  //
  nScreenWidth, nScreenHeight: integer;
  ScreenDc, hCaptureDC: HDC;
  l_capture_rect: TRect;
  last_message_sent: TDateTime;

  // for window capture
  window_rect: TRect;
  start_point: TPoint;
  wp: WINDOWPLACEMENT;
  tmp_val: boolean;
begin
  finished := false;
  last_message_sent := 0;

  while not Terminated do
  begin
    tmp_frame := nil;

    local_list := free_frames.LockList;
    if local_list.Count > 0 then
    begin
      tmp_frame := TRGB_frame(local_list.Items[0]);
      local_list.Delete(0);
    end;
    free_frames.UnlockList;

    if Assigned(tmp_frame) then
    begin
      nScreenWidth := GetSystemMetrics(SM_CXSCREEN);
      nScreenHeight := GetSystemMetrics(SM_CYSCREEN);

      // load default
      l_capture_rect.Left := 0;
      l_capture_rect.Top := 0;
      l_capture_rect.width := nScreenWidth;
      l_capture_rect.height := nScreenHeight;

      // region ?
      if capture_mode = 1 then
      begin
        l_capture_rect := capture_rect;
        if l_capture_rect.Right >= nScreenWidth then
          l_capture_rect.Right := nScreenWidth - 1;
        if l_capture_rect.Bottom >= nScreenHeight then
          l_capture_rect.Bottom := nScreenHeight - 1;
      end
      else
      begin
        // window?
        if capture_mode in [2, 3] then
        begin
          if IsWindow(capture_window) then
          begin
            if IsWindowVisible(capture_window) then
            begin
              if capture_mode = 2 then
              begin
                // window
                if GetWindowRect(capture_window, window_rect) then
                  if (window_rect.Left > 0) and (window_rect.Right > 0) then
                    l_capture_rect := window_rect;
              end
              else
              begin
                // window's client area
                start_point.SetLocation(0, 0);
                if GetClientRect(capture_window, window_rect) and
                  ClientToScreen(capture_window, start_point) then
                  if (window_rect.Left > 0) and (window_rect.Right > 0) then
                  begin
                    l_capture_rect := window_rect;
                    l_capture_rect.SetLocation(start_point);
                  end;
              end;
            end;
          end
          else
          begin
            // sent notification if no target window
            if MillisecondsBetween(last_message_sent, Now()) > 1000 then
            begin
              Winapi.Windows.PostMessage(mothers_window,
                WM_WINDOW_MISSED, 0, 0);
              last_message_sent := Now();
            end;
          end;
        end;
      end;

      ScreenDc := GetDC(GetDesktopWindow);
      hCaptureDC := CreateCompatibleDC(ScreenDc);

      bmp := TBitMap.Create;
      bmp.Handle := CreateCompatibleBitmap(ScreenDc, l_capture_rect.width,
        l_capture_rect.height);
      SelectObject(hCaptureDC, bmp.Handle);

      BitBlt(hCaptureDC, 0, 0, l_capture_rect.width, l_capture_rect.height,
        ScreenDc, l_capture_rect.Left, l_capture_rect.Top, SRCCOPY);

      bmp.HandleType := bmDIB;
      // bmp.PixelFormat := pf32bit;

      tmp_frame.Fill(l_capture_rect.width, l_capture_rect.height, bmp);
      filled_frames.Add(tmp_frame);

      ReleaseDC(GetDesktopWindow, ScreenDc);
      DeleteDC(hCaptureDC);
      bmp.Free;
    end;
    Sleep(1);
  end;

  finished := true;
end;

function ScreenShot(hWindow: hWnd; bm: TBitMap): boolean;
var
  R: TRect;
  ScreenDc: HDC;
  lpPal: PLOGPALETTE;
  wp: WINDOWPLACEMENT;
  ai: ANIMATIONINFO;
  restoreAnimation: boolean;
  ExStyle: LONG_PTR;
begin
  Result := false;
  if not IsWindow(hWindow) then
    Exit;

  ZeroMemory(@wp, SizeOf(wp));
  wp.length := SizeOf(wp);
  GetWindowPlacement(hWindow, @wp);

  ZeroMemory(@ai, SizeOf(ai));
  restoreAnimation := false;

  if wp.showCmd = SW_SHOWMINIMIZED then
  begin
    ai.cbSize := SizeOf(ai);
    SystemParametersInfo(SPI_GETANIMATION, SizeOf(ai), @ai, 0);

    if ai.iMinAnimate <> 0 then
    begin
      ai.iMinAnimate := 0;
      SystemParametersInfo(SPI_SETANIMATION, SizeOf(ai), @ai, 0);
      restoreAnimation := true;
    end;

    ExStyle := GetWindowLongPtr(hWindow, GWL_EXSTYLE);
    if (ExStyle and WS_EX_LAYERED) <> WS_EX_LAYERED then
    begin
      SetWindowLongPtr(hWindow, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);
    end;
    SetLayeredWindowAttributes(hWindow, 0, 1, LWA_ALPHA);

    ShowWindow(hWindow, SW_SHOWNOACTIVATE);
  end;

  GetWindowRect(hWindow, R);
  bm.width := R.Right - R.Left;
  bm.height := R.Bottom - R.Top;

  ScreenDc := GetDC(0);

  if (GetDeviceCaps(ScreenDc, RASTERCAPS) and RC_PALETTE) = RC_PALETTE then
  begin
    GetMem(lpPal, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));
    ZeroMemory(lpPal, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));
    lpPal^.palVersion := $300;
    lpPal^.palNumEntries := GetSystemPaletteEntries(ScreenDc, 0, 256,
      lpPal^.palPalEntry);
    if lpPal^.palNumEntries <> 0 then
    begin
      bm.Palette := CreatePalette(lpPal^);
    end;
    FreeMem(lpPal, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));
  end;

  BitBlt(bm.Canvas.Handle, 0, 0, bm.width, bm.height, ScreenDc, R.Left,
    R.Top, SRCCOPY);
  ReleaseDC(0, ScreenDc);

  if (wp.showCmd = SW_SHOWMINIMIZED) then
  begin
    SetWindowPlacement(hWindow, @wp);

    SetLayeredWindowAttributes(hWindow, 0, 255, LWA_ALPHA);
    if (ExStyle and WS_EX_LAYERED) <> WS_EX_LAYERED then
    begin
      SetWindowLongPtr(hWindow, GWL_EXSTYLE, ExStyle);
    end;

    if restoreAnimation then
    begin
      ai.iMinAnimate := 1;
      SystemParametersInfo(SPI_SETANIMATION, SizeOf(ANIMATIONINFO), @ai, 0);
    end;
  end;

  Result := true;
end;

end.
