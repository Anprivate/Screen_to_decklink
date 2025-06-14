var
  ScreenForm: TFormSelect;
begin
  // создаем нашу полупрозрачную форму
  ScreenForm := TFormSelect.Create(Self);
  try
    // и раст¤гиваем еЄ на весь экран
    ScreenForm.Width := Screen.DesktopWidth;
    ScreenForm.Height := Screen.DesktopHeight;
    ScreenForm.Left := 0;
    ScreenForm.Top := 0;

    ScreenForm.SetInitialRect(100, 100, 500, 500);

    // дальше пр¤чем основную форму
    Self.Hide;

    // показываем полупрозрачную ФзаливкуФ
    ScreenForm.ShowModal;
    {
      // и выводим полученную область экрана
      Image1.Picture.BitMap := ScreenForm.Bild;
      ScrollBox1.HorzScrollBar.Range := Image1.Picture.Width;
      ScrollBox1.VertScrollBar.Range := Image1.Picture.Height;
    }
    Self.Show;
  finally
    ScreenForm.Free;
  end;