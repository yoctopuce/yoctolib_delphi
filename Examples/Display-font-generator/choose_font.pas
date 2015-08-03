unit choose_font;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFontChooser = class(TForm)
    ComboBox1: TComboBox;
    fontsizeinput: TEdit;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    bold: TCheckBox;
    italic: TCheckBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure fontsizeinputChange(Sender: TObject);
    procedure boldClick(Sender: TObject);
    procedure italicClick(Sender: TObject);
  private
    { Private declarations }
    _fontname  : string;
    _success   : boolean;
    _italic    : boolean;
    _bold      : boolean;
    _size      : integer;
    bm         : tbitmap;
  public
    { Public declarations }
    function success(): boolean;
    function fontname():string;
    function fontsize():integer;
    function isItalic():boolean;
    function isbold():boolean;


  end;

var
  FontChooser: TFontChooser;

implementation

{$R *.DFM}

procedure TFontChooser.ComboBox1Change(Sender: TObject);
begin
 bm.width  := image1.width;
 bm.height := image1.height;
 _size     := strtoint(fontsizeinput.text);
 if (_size<0) then _size:= 8;
 bm.canvas.Brush.color := clBtnFace;
 bm.canvas.FillRect(rect(0,0, bm.width, bm.height));
 bm.Canvas.Font.name :=  ComboBox1.Items[ComboBox1.itemindex];
 bm.Canvas.Font.size :=  _size;

 if bold.checked   then  bm.Canvas.Font.style :=  bm.Canvas.Font.style+[fsBold]
                 else  bm.Canvas.Font.style :=  bm.Canvas.Font.style-[fsBold];
 if italic.checked then  bm.Canvas.Font.style  :=  bm.Canvas.Font.style+[fsItalic]
                 else  bm.Canvas.Font.style :=  bm.Canvas.Font.style-[fsItalic];
 bm.canvas.TextOut(0,0,'ABcd 0123 Hello!');

 image1.picture.assign(bm);

 
end;

procedure TFontChooser.FormCreate(Sender: TObject);
begin
   bm := tbitmap .create;
   ComboBox1.Items.AddStrings(Screen.Fonts);
   ComboBox1.itemindex:=0;
   ComboBox1Change(nil);
end;

procedure TFontChooser.Button1Click(Sender: TObject);
begin
    _fontname := ComboBox1.Items[ComboBox1.itemindex];
    _success  := true;
    _size     := strtoint(fontsizeinput.text);
    _italic   := italic.checked;
    _bold     := bold.checked;
    if (_size<0) then _size:= 8;
    close();
end;

procedure TFontChooser.Button2Click(Sender: TObject);
begin
    _fontname := '';
    _success  := false;
    close();
end;

function TFontChooser.success(): boolean;
  begin
     success:=_success;
  end;

function TFontChooser.fontname():string;
 begin
     fontname:=_fontname;
  end;

function TFontChooser.fontsize():integer;
 begin
     fontsize:=_size;
  end;

function TFontChooser.isItalic():boolean;
 begin
     isItalic:=_italic;
  end;

function TFontChooser.isbold():boolean;
 begin
     isbold:=_bold;
  end;

procedure TFontChooser.fontsizeinputChange(Sender: TObject);
begin
  ComboBox1Change(nil);
end;

procedure TFontChooser.boldClick(Sender: TObject);
begin
ComboBox1Change(nil);
end;

procedure TFontChooser.italicClick(Sender: TObject);
begin
ComboBox1Change(nil);
end;

end.
