unit Unit_window_select;

interface

uses
  Winapi.Windows, Winapi.Messages,
  WinTypes, WinProcs,
  System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TFormWindowSelect = class(TForm)
    ListBoxForms: TListBox;
    BitBtn1: TBitBtn;
    BitBtnCancel: TBitBtn;
    Label1: TLabel;
    EditSelected: TEdit;
    procedure FormShow(Sender: TObject);
    procedure ListBoxFormsClick(Sender: TObject);
  private
  public
    WindowName: string;
  end;

var
  FormWindowSelect: TFormWindowSelect;

implementation

{$R *.dfm}

function enumListOfTasks(hWindow: hWnd; param: lParam): Bool; stdcall;
var
  HoldString: PWideChar;
  WinName: string;
  WindowStyle: Longint;
  IsAChild, IsVisible, IsApp: Bool;

begin
  WindowStyle := GetWindowLong(hWindow, GWL_STYLE);

  IsVisible := (WindowStyle and Longint(WS_VISIBLE)) <> 0;
  IsApp := (WindowStyle and Longint(WS_EX_APPWINDOW)) <> 0;

  IsAChild := GetWindowLong(hWindow, GWL_HWNDPARENT) <> 0;

  GetMem(HoldString, 256);
  if GetWindowText(hWindow, HoldString, 255) > 0 then
    WinName := StrPas(HoldString);
  FreeMem(HoldString, 256);

  if IsVisible and IsApp and not IsAChild and (Length(WinName) > 0) then
    FormWindowSelect.ListBoxForms.Items.Add(WinName);

  Result := TRUE;
end;

procedure TFormWindowSelect.FormShow(Sender: TObject);
begin
  ListBoxForms.Clear;

  enumWindows(@enumListOfTasks, 0);

  EditSelected.Text := WindowName;

  if (ListBoxForms.Items.Count > 0) then
  begin
    ListBoxForms.ItemIndex := 0;
  end;
end;

procedure TFormWindowSelect.ListBoxFormsClick(Sender: TObject);
begin
  EditSelected.Text := ListBoxForms.Items[ListBoxForms.ItemIndex];
  WindowName := EditSelected.Text;
end;

end.
