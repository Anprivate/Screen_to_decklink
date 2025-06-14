unit Unit_scaler_thread;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics, System.Generics.Collections,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libpostproc,
  libswresample,
  libswscale,
  unit_capturer_thread;

type
  TYuv_frame = class(TObject)
  private
    l_width, l_height: integer;
    l_data: Pointer;
    l_data_size: integer;
  public
    property width: integer read l_width;
    property height: integer read l_height;
    property data: Pointer read l_data;
    property data_size: integer read l_data_size;
    //
    Constructor Create(in_width, in_height: integer; in_data: Pointer);
    Destructor Destroy; override;
  end;

  Tscaler_thread = class(TThread)
  private
    procedure filtering(tmp_yuv_frame: TYuv_frame);
  public
    free_rgb_frames: TThreadList<TRGB_frame>;
    filled_rgb_frames: TThreadList<TRGB_frame>;
    out_frames: TThreadList<TYuv_frame>;
    max_frames_in_list: integer;

    width, height: integer;
    filter: integer;
    finished: boolean;
  protected
    procedure Execute; override;
  end;

implementation

{ TYuv_frame }
constructor TYuv_frame.Create(in_width, in_height: integer; in_data: Pointer);
begin
  inherited Create;

  l_width := in_width;
  l_height := in_height;
  l_data_size := l_width * l_height * 2;
  l_data := GetMemory(l_data_size);
  CopyMemory(l_data, in_data, l_data_size);
end;

destructor TYuv_frame.Destroy;
begin
  if Assigned(data) then
    FreeMemory(data);

  inherited;
end;

{ Tscaler_thread }
procedure Tscaler_thread.Execute;
var
  local_in_list: TList<TRGB_frame>;
  local_out_list: TList<TYUV_frame>;
  fiol: integer;

  src_data, dst_data: array [0 .. 3] of PByte;
  src_linesize, dst_linesize: array [0 .. 3] of integer;
  src_w, src_h, dst_w, dst_h: integer;
  src_pix_fmt, dst_pix_fmt: AVPixelFormat;
  dst_bufsize: integer;
  sws_ctx: PSwsContext;

  tmp_yuv_frame: TYuv_frame;
  tmp_rgb_frame: TRGB_frame;
  y: integer;
  src_ptr, dst_ptr: PByte;
  src_line_size: integer;
label
  failed;
begin
  finished := false;

  while not Terminated do
  begin
    local_out_list := out_frames.LockList;
    fiol := local_out_list.Count;
    out_frames.UnlockList;

    if fiol < max_frames_in_list then
    begin
      tmp_rgb_frame := nil;

      local_in_list := filled_rgb_frames.LockList;
      if local_in_list.Count > 0 then
      begin
        tmp_rgb_frame := local_in_list.Items[0];
        local_in_list.Delete(0);
      end;
      filled_rgb_frames.UnlockList;

      if Assigned(tmp_rgb_frame) then
      begin
        src_w := tmp_rgb_frame.width;
        src_h := tmp_rgb_frame.height;
        case tmp_rgb_frame.bpp of
          2:
            src_pix_fmt := AV_PIX_FMT_RGB555LE;
          3:
            src_pix_fmt := AV_PIX_FMT_BGR24;
          4:
            src_pix_fmt := AV_PIX_FMT_BGRA;
        else
          src_pix_fmt := AV_PIX_FMT_BGR24;
        end;

        dst_w := width;
        dst_h := height;
        dst_pix_fmt := AV_PIX_FMT_UYVY422;

        // create scaling context
        sws_ctx := sws_getContext(src_w, src_h, src_pix_fmt, dst_w, dst_h,
          dst_pix_fmt, SWS_BILINEAR, nil, nil, nil);
        if not Assigned(sws_ctx) then
          goto failed;

        // allocate source image buffer
        if av_image_alloc(@src_data[0], @src_linesize[0], src_w, src_h,
          src_pix_fmt, 16) < 0 then
          goto failed;

        src_ptr := tmp_rgb_frame.data;
        dst_ptr := src_data[0];
        src_line_size := src_w * tmp_rgb_frame.bpp;

        for y := 0 to src_h - 1 do
        begin
          CopyMemory(dst_ptr, src_ptr, src_line_size);
          inc(src_ptr, src_line_size);
          inc(dst_ptr, src_linesize[0]);
        end;

        // allocate destination image buffer
        dst_bufsize := av_image_alloc(@dst_data[0], @dst_linesize[0], dst_w,
          dst_h, dst_pix_fmt, 1);
        if dst_bufsize < 0 then
          goto failed;

        // convert to destination format
        sws_scale(sws_ctx, @src_data[0], @src_linesize[0], 0, src_h,
          @dst_data[0], @dst_linesize[0]);

        tmp_yuv_frame := TYuv_frame.Create(dst_w, dst_h, dst_data[0]);

        filtering(tmp_yuv_frame);

        out_frames.Add(tmp_yuv_frame);
      failed:
        av_freep(@src_data[0]);
        av_freep(@dst_data[0]);
        sws_freeContext(sws_ctx);

        free_rgb_frames.Add(tmp_rgb_frame);
      end;
    end;

    Sleep(5);
  end;

  finished := true;
end;

procedure Tscaler_thread.filtering(tmp_yuv_frame: TYuv_frame);
var
  prev_line_ptr, cur_line_ptr, next_line_ptr: PByte;
  out_ptr, cur_out_ptr: PByte;
  row_length: integer;
  iX, iY: integer;
  accum: Uint16;
begin
  if filter = 0 then
    Exit;

  row_length := tmp_yuv_frame.width * 2;
  out_ptr := GetMemory(tmp_yuv_frame.data_size);
  cur_out_ptr := out_ptr;

  cur_line_ptr := tmp_yuv_frame.data;
  prev_line_ptr := cur_line_ptr;

  for iX := 0 to row_length - 1 do
  begin
    cur_out_ptr^ := cur_line_ptr^;
    inc(cur_out_ptr);
    inc(cur_line_ptr);
  end;

  next_line_ptr := cur_line_ptr + row_length;

  if filter = 1 then // soft 0.75+0.125+0.125
  begin
    for iY := 1 to tmp_yuv_frame.height - 1 do
    begin
      for iX := 0 to row_length - 1 do
      begin
        accum := cur_line_ptr^ shl 2;
        inc(accum, cur_line_ptr^ shl 1);
        inc(accum, prev_line_ptr^);
        inc(accum, next_line_ptr^);
        cur_out_ptr^ := byte(accum shr 3);

        inc(cur_out_ptr);
        inc(prev_line_ptr);
        inc(cur_line_ptr);
        inc(next_line_ptr);
      end;
    end;
  end;

  if filter = 2 then // strong 0.5+0.25+0.25
  begin
    for iY := 1 to tmp_yuv_frame.height - 1 do
    begin
      for iX := 0 to row_length - 1 do
      begin
        accum := cur_line_ptr^ shl 1;
        inc(accum, prev_line_ptr^);
        inc(accum, next_line_ptr^);
        cur_out_ptr^ := byte(accum shr 2);

        inc(cur_out_ptr);
        inc(prev_line_ptr);
        inc(cur_line_ptr);
        inc(next_line_ptr);
      end;
    end;
  end;

  for iX := 0 to row_length - 1 do
  begin
    cur_out_ptr^ := cur_line_ptr^;
    inc(cur_out_ptr);
    inc(cur_line_ptr);
  end;

  // tmp_yuv_frame.l_data;
  CopyMemory(tmp_yuv_frame.data, out_ptr, tmp_yuv_frame.data_size);
  FreeMemory(out_ptr);
end;

end.
