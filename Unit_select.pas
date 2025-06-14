unit Unit_select;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, System.Types, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TFormSelect = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    isDown: boolean;
    downX, downY: Integer;
    tmpRect: TRect;
    procedure DrawSelected(X1, Y1, X2, Y2: Integer);
    procedure ValidateXY(var X, Y: Integer);
  public
    Selection: TRect;
    ARmode: Integer;
    procedure SetInitialRect(X1, Y1, X2, Y2: Integer);
  end;

var
  FormSelect: TFormSelect;

implementation

{$R *.dfm}

procedure TFormSelect.DrawSelected(X1, Y1, X2, Y2: Integer);
begin
  // перерисовываем форму
  Self.Repaint;

  // тут мы рисуем  пунктирную рамку красного цвета
  Self.Canvas.Pen.Color := clRed;
  Self.Canvas.Pen.Width := 1;

  Self.Canvas.Pen.Style := psDot;

  Self.Canvas.Polyline([Point(X1, Y1), Point(X1, Y2), Point(X2, Y2),
    Point(X2, Y1), Point(X1, Y1)]);
end;

procedure TFormSelect.FormActivate(Sender: TObject);
begin
  DrawSelected(tmpRect.Left, tmpRect.Top, tmpRect.Right, tmpRect.Bottom);
end;

procedure TFormSelect.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Self.Close;

  if Key = VK_RETURN then
  begin
    Selection := tmpRect;
    Self.Close;
  end;
end;

procedure TFormSelect.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // устанавливаем флаг нажатия мыши в true
  isDown := true;

  // и запоминаем текущие координаты
  downX := X;
  downY := Y;
end;

procedure TFormSelect.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  tmpX, tmpY: Integer;
begin
  // если нажата клавиша мыши, то мы рисуем рамку выделения
  if isDown then
  begin
    tmpX := X;
    tmpY := Y;

    ValidateXY(tmpX, tmpY);

    DrawSelected(downX, downY, tmpX, tmpY);
  end;
end;

procedure TFormSelect.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tmpX, tmpY: Integer;
begin
  // сбрасываем флаг
  isDown := false;

  tmpX := X;
  tmpY := Y;

  ValidateXY(tmpX, tmpY);

  DrawSelected(downX, downY, tmpX, tmpY);

  // сохраняем координаты области
  tmpRect.Left := downX;
  tmpRect.Top := downY;
  tmpRect.Right := tmpX;
  tmpRect.Bottom := tmpY;
end;

procedure TFormSelect.FormPaint(Sender: TObject);
begin
  Self.Canvas.Font.Height := Self.Width div 50;
  Self.Canvas.TextOut(50, 50,
    'New selection: ENTER - for save / ESC - for cancel');
end;

procedure TFormSelect.SetInitialRect(X1, Y1, X2, Y2: Integer);
begin
  tmpRect.Left := X1;
  tmpRect.Top := Y1;
  tmpRect.Right := X2;
  tmpRect.Bottom := Y2;

  Selection := tmpRect;
end;

procedure TFormSelect.ValidateXY(var X, Y: Integer);
var
  tmpX, tmpY: Integer;
  reval: boolean;
begin
  tmpX := X;
  tmpY := Y;

  case ARmode of
    1:
      tmpY := downY + ABS(X - downX) * 3 div 4 * SIGN(Y - downY);
    2:
      tmpY := downY + ABS(X - downX) * 9 div 16 * SIGN(Y - downY);
  end;

  reval := false;

  if tmpY < 0 then
  begin
    tmpY := 0;
    reval := true;
  end;

  if tmpY >= Screen.Height then
  begin
    tmpY := Screen.Height - 1;
    reval := true;
  end;

  if reval then
    case ARmode of
      1:
        tmpX := downX + ABS(tmpY - downY) * 4 div 3 * SIGN(X - downX);
      2:
        tmpX := downX + ABS(tmpY - downY) * 16 div 9 * SIGN(X - downX);
    end;

  X := tmpX;
  Y := tmpY;
end;

end.
