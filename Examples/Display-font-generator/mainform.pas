unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus,kernel, ComCtrls;

type
  abyte = array [0..256] of byte;
  pbyte = ^abyte;
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    loadfrombmp: TOpenDialog;
    bmpsave: TSaveDialog;
    MainMenu1: TMainMenu;
    Import1: TMenuItem;
    ExportMenu: TMenuItem;
    FromSystemFont1: TMenuItem;
    FromyfFile1: TMenuItem;
    Fromimagefile1: TMenuItem;
    Toyffile1: TMenuItem;
    Toimagefile1: TMenuItem;
    YfSave: TSaveDialog;
    openYf: TOpenDialog;
    ZoomMenu: TMenuItem;
    N1001: TMenuItem;
    N2001: TMenuItem;
    N3001: TMenuItem;
    N4001: TMenuItem;
    N5001: TMenuItem;
    N6001: TMenuItem;
    x71: TMenuItem;
    x81: TMenuItem;
    x91: TMenuItem;
    x101: TMenuItem;
    StatusBar1: TStatusBar;
    PreviewMenu: TMenuItem;
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);


    procedure Button2Click(Sender: TObject);


    procedure FromSystemFont1Click(Sender: TObject);
    procedure Toyffile1Click(Sender: TObject);
    procedure FromyfFile1Click(Sender: TObject);
    procedure Toimagefile1Click(Sender: TObject);
    procedure Fromimagefile1Click(Sender: TObject);

    Procedure applyZoom();
    procedure N1001Click(Sender: TObject);
    procedure N2001Click(Sender: TObject);
    procedure N3001Click(Sender: TObject);
    procedure N5001Click(Sender: TObject);
    procedure N4001Click(Sender: TObject);
    procedure N6001Click(Sender: TObject);
    procedure x71Click(Sender: TObject);
    procedure x81Click(Sender: TObject);
    procedure x91Click(Sender: TObject);
    procedure x101Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);


    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PreviewMenuClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);


  private
    { Private declarations }
    bm:tbitmap;
    zoom:integer;
    previewData : barray;
    previewDataWidth : integer;
    procedure preview();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses choose_font, preview;

{$R *.DFM}

var
refString: string;

procedure TForm1.FormCreate(Sender: TObject);
 var i:integer;

begin
 bm := TBitmap.create();
   refString :='';
   for i:=32 to 255 do refString:=refString+chr(i);
  zoom:=1;

end;



procedure TForm1.Button2Click(Sender: TObject);
 begin
  //Memo1.text:= BitmapToYfData(image1.picture.bitmap,nil);
 end;







procedure TForm1.FromSystemFont1Click(Sender: TObject);
  var


  baseline:integer;
begin
  FontChooser.showmodal;
  if  FontChooser.success() then
   begin

    SystemFontToBitmap(bm,refString,  FontChooser.fontname(),FontChooser.fontsize(),FontChooser.isItalic(),FontChooser.isBold(),  baseline);
    image1.width:=bm.width;
    image1.height:=bm.height;
    image1.picture:=tpicture(bm);
    ZoomMenu.enabled:=true;
    ExportMenu.enabled:=true;
    PreviewMenu.enabled:=true;
   end;


end;

procedure TForm1.Toyffile1Click(Sender: TObject);
var
  mem:Tmemorystream;
begin
  if YfSave.execute() then
   begin
    mem:=Tmemorystream.create;
    BitmapToYfData(image1.picture.bitmap,mem);
    mem.savetofile(YfSave.FileName);
    mem.free();
   end;
end;

procedure TForm1.FromyfFile1Click(Sender: TObject);
var

   mem:Tmemorystream;
begin
  if openYf.execute() then
  begin
      mem:=Tmemorystream.create;
      mem.loadFromfile(openYf.filename);
      YfDataToBitmap(mem,bm);
      image1.width:=bm.width*zoom;
      image1.height:=bm.height*zoom;
      image1.picture:=tpicture(bm);
      mem.free();
      ExportMenu.enabled:=true;
      ZoomMenu.enabled:=true;
      PreviewMenu.enabled:=true;
  end;
end;

procedure TForm1.Toimagefile1Click(Sender: TObject);
begin
  if bmpsave.execute() then
   begin
    image1.picture.bitmap.savetofile(bmpsave.filename);
   end;
end;

procedure TForm1.Fromimagefile1Click(Sender: TObject);
begin
  if loadfrombmp.execute then
   begin
    bm.loadfromfile(loadfrombmp.filename);
    image1.picture:=tpicture(bm);
    image1.width:=bm.width*zoom;
    image1.height:=bm.height*zoom;
    ExportMenu.enabled:=true;
    ZoomMenu.enabled:=true;
    PreviewMenu.enableD:=true
   end;
end;

Procedure Tform1.applyZoom();
 begin
  image1.width:=bm.width*zoom;
  image1.height:=bm.height*zoom;
  image1.picture:=tpicture(bm);
  case zoom of
   1: n1001.checked:=true;
   2: n2001.checked:=true;
   3: n3001.checked:=true;
   4: n5001.checked:=true;
   5: n4001.checked:=true;
   6: n6001.checked:=true;
   7: x71.checked:=true;
   8: x81.checked:=true;
   9: x91.checked:=true;
   10: x101.checked:=true;
 end;

 end;



procedure TForm1.N1001Click(Sender: TObject);
begin
 zoom:=1;applyZoom();
end;

procedure TForm1.N2001Click(Sender: TObject);
begin
zoom:=2;applyZoom();
end;

procedure TForm1.N3001Click(Sender: TObject);
begin
zoom:=3;applyZoom();
end;

procedure TForm1.N5001Click(Sender: TObject);
begin
zoom:=5;applyZoom();
end;

procedure TForm1.N4001Click(Sender: TObject);
begin
zoom:=4;applyZoom();
end;

procedure TForm1.N6001Click(Sender: TObject);
begin
zoom:=6;applyZoom();
end;

procedure TForm1.x71Click(Sender: TObject);
begin
zoom:=7;applyZoom();
end;

procedure TForm1.x81Click(Sender: TObject);
begin
zoom:=8;applyZoom();
end;

procedure TForm1.x91Click(Sender: TObject);
begin
zoom:=9;applyZoom();
end;

procedure TForm1.x101Click(Sender: TObject);
begin
zoom:=10;applyZoom();
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
   i,j:integer;
   continue : boolean;
   tmp:tbitmap;
begin
   x := trunc(x / zoom);
   y := trunc(y / zoom);
   if ssCtrl in Shift then
     begin
      if Button=mbLeft then
      begin
        tmp:=tbitmap.create();
        tmp.width := bm.width;
        tmp.height := bm.height;
        tmp.canvas.copyrect(rect(0,0,bm.width,bm.height),bm.canvas, rect(0,0,bm.width,bm.height));

        bm.canvas.copyrect(rect(X,0,bm.width-1,bm.height),tmp.canvas, rect(X+1,0,bm.width,bm.height));
        tmp.free;
         for i:=0 to bm.height-1 do
           bm.canvas.pixels[bm.width-1,i]:=$ffffff

      end;
      if Button=mbright then
       begin
        bm.width := (bm.width +1 + $f) and $FFFFFF0;
        tmp:=tbitmap.create();
        tmp.width := bm.width;
        tmp.height := bm.height;
        tmp.canvas.copyrect(rect(0,0,bm.width,bm.height),bm.canvas, rect(0,0,bm.width,bm.height));
        bm.canvas.copyrect(rect(X+1,0,bm.width-1,bm.height),tmp.canvas, rect(X,0,bm.width-2,bm.height));
        tmp.free;
        for i:=0 to bm.height-1 do
           bm.canvas.pixels[x,i]:=$ffffff
       end;



      continue := true;
      while (bm.width>0) and continue do
        begin
           for j:=0 to bm.height-1  do
             for i:=bm.width-16 to bm.width-1 do
               if bm.canvas.pixels[i,j]=0 then continue:=false;
           if (continue) then
             bm.width := bm.width - 16;
        end;

     end
   else
   begin
     if bm.canvas.pixels[x,y] = 0  then   bm.canvas.pixels[x,y]:=$ffffff
                          else    bm.canvas.pixels[x,y]:=0;

   end;
  image1.picture:=tpicture(bm);
   image1.width:= bm.width*zoom;
end;





procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  x,y :integer;
  p : TPoint;
begin
   p:=ScreenToClient(MousePos);
   x := round((p.x + scrollbox1.HorzScrollBar.Position) /zoom);
   y := round((p.y + scrollbox1.VertScrollBar.Position) /zoom);
   if zoom>1 then dec(zoom);
   applyZoom();
   scrollbox1.HorzScrollBar.Position := (zoom * x) - p.x;
   scrollbox1.VertScrollBar.Position := (zoom * y) - p.y;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  x,y :integer;
  p : TPoint;
 begin
   p:=ScreenToClient(MousePos);

   x := round((p.x + scrollbox1.HorzScrollBar.Position) /zoom);
   y := round((p.y + scrollbox1.VertScrollBar.Position) /zoom);
   if zoom<10 then inc(zoom);

   scrollbox1.HorzScrollBar.Position := (zoom * x) - p.x;
   scrollbox1.VertScrollBar.Position := (zoom * y) - p.y;


  applyZoom();
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var
   asciicode,i :integer;
begin

  x:= trunc(x /zoom);
  Y:= trunc(y /zoom);
  if x>=0 then
   begin
    asciicode := 32;
    if (x>=image1.picture.bitmap.width) then x:= image1.picture.bitmap.width-1;
    for i:=0 to x-1 do if image1.picture.bitmap.canvas.pixels[i,0]=0 then inc(asciicode);
    statusbar1.panels[0].text := inttostr(x)+','+inttostr(y);
    statusbar1.panels[1].text := ' ascii: '+inttostr(asciicode);
    statusbar1.panels[2].text := chr(asciicode);
   end else statusbar1.panels[0].text :='';

end;

procedure TForm1.ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   statusbar1.panels[0].text := '';
   statusbar1.panels[1].text := '';
   statusbar1.panels[2].text := '';
end;

procedure TForm1.PreviewMenuClick(Sender: TObject);
begin
  if not(previewForm.visible) then previewForm.show();
end;

Procedure TForm1.preview();
var
   msg :string;
begin
 if previewForm.visible then
   begin
     msg := previewForm.getMessage();
     ConstructBitmap(bm,msg,previewData,previewDataWidth);
     previewForm.setPreviewData(previewData,previewDataWidth);
   end;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
 preview();
end;

end.
