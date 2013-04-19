program font_generator;

uses
  Forms,
  mainform in 'mainform.pas' {Form1},
  choose_font in 'choose_font.pas' {FontChooser},
  kernel in 'kernel.pas',
  preview in 'preview.pas' {previewForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Yoctopuce Font Generator';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFontChooser, FontChooser);
  Application.CreateForm(TpreviewForm, previewForm);
  Application.Run;
end.
