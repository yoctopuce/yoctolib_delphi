unit kernel;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics;

type
  barray =  Array of byte;

Procedure SystemFontToBitmap(var bm:Tbitmap; refString :string; fontname:string;fontSize:integer; italic,bold:boolean; var baseline:integer);
function  BitmapToYfData( bm:Tbitmap;datafile:tmemorystream):string;
procedure YfDataToBitmap(mem:tmemorystream;bm:tbitmap);
Procedure ConstructBitmap(bm :Tbitmap; msg :string ;  var data : barray; var datawidth: integer );

implementation


Procedure ConstructBitmap(bm :Tbitmap; msg :string ; var data :barray; var datawidth: integer );
 var
   charposition : array[0..255] of integer;
   charwidth : array[0..255] of integer;
   lastpixel    : integer;
   ascii,i,j,k,bmheight        : integer;
   bytesperline,datasize : integer;
   offsetx,w,offset,x,index : integer;

 begin
   ascii     := 32;
   lastpixel := 0;
   for i:=0 to 255 do
    begin
     charposition[i]:=0;
     charwidth[i]:=0;
    end;

   for i:=0 to bm.width-1 do
     begin
       if (bm.canvas.pixels[i,0] =0) then
        if (ascii<=255) then
          begin
            charposition[ascii]:=lastpixel;
            charwidth[ascii]:= i - lastpixel +1;
            lastpixel:=i+1;
            inc(ascii);
          end;
     end;

   datawidth := 0;
   for i:=1 to length(msg) do
     inc(datawidth,charwidth[ord(msg[i])]);

   bmheight:=  bm.height-1;
   bytesperline := (datawidth+7) div 8;
   datasize :=  bytesperline * bmheight;
   setlength(data,datasize);
   for i:=0 to datasize-1 do data[i]:=0;

   offsetx := 0 ;
   for k:=1 to length(msg) do
     begin
      w      :=    charwidth[ord(msg[k])];
      offset :=    charposition[ord(msg[k])];
      for j:=0 to bmheight-1 do
        for i:=0 to  w-1 do
          if (bm.canvas.pixels[offset+i,j+1] =0) then
            begin
               x := offsetx + i;
               index:= j *bytesperline + x div 8;
               data[index] := data[index] or  (128 shr (x mod 8));
            end;
      inc( offsetx,w);
   end;


 end;

Procedure SystemFontToBitmap(var bm:Tbitmap; refString :string; fontname:string;fontSize:integer;italic,bold:boolean; var baseline:integer);
 var
  w,h,i,x,cw: integer;
  TM: TextMetric;
  DC: HDC;
  SaveFont: HFont;

begin
    bm.canvas.font.name :=  fontname;
    bm.canvas.font.size :=  fontSize;

    if bold   then  bm.Canvas.Font.style :=  bm.Canvas.Font.style+[fsBold]
                 else  bm.Canvas.Font.style :=  bm.Canvas.Font.style-[fsBold];
    if italic then  bm.Canvas.Font.style  :=  bm.Canvas.Font.style+[fsItalic]
                 else  bm.Canvas.Font.style :=  bm.Canvas.Font.style-[fsItalic];

    DC := GetDC(0);
    SaveFont := SelectObject(DC, bm.canvas.font.Handle);
    GetTextMetrics(DC, TM);
    SelectObject(DC, SaveFont);

    ReleaseDC(0, DC);

    w :=0;
    for i:=1 to length(refString) do
      w := w+ bm.canvas.TextWidth(refString[i])+1;


    w:=(w+15) and $fffff0;
    h :=  bm.canvas.TextHeight(refString)+1;

    bm.width := w;
    bm.height:=h;
    bm.canvas.Brush.color:=$ffffff;
    bm.canvas.fillrect(rect(0,0,w,h)) ;

    bm.canvas.pen.color:=0;
    bm.Monochrome := true;
    bm.canvas.brush.style:=bsclear ;

    //bm.canvas.pixels[0,TM.tmAscent]:= 0;
    baseline:= w- TM.tmAscent;
    x:=0;
    for i:=1 to length(refString) do
     begin
      bm.canvas.TextOut(x,1,refString[i]);
      cw := bm.canvas.TextWidth(refString[i]);
      x := x + cw ;
      bm.canvas.moveto(x,0);
      bm.canvas.lineto(x,1);
      inc(x);
     end;
 end;

function BitmapToYfData( bm:Tbitmap; datafile:tmemorystream):string;

var
 first  : char;
 found:integer;

 b:byte;
 i,j,k : integer;
 text:string;
 baseline:integer;
 textsize:integer;
 textlinesize:integer;
  color:integer;
  baselineCounter: array of integer;
 max:integer;

Procedure PrintByte(i:byte) ;
 begin
  if  (textsize>0) then text := text+',';
  if (textlinesize>=16) then
   begin
    text := text+#13#10;
    textlinesize := 0;
   end;

  text := text+'0x'+IntToHex(i,2);
  if datafile<>nil then datafile.WriteBuffer(i,1);

  inc(textsize);
  inc(textlinesize)
 end;

Procedure PrintInteger(i:integer);
 begin
    PrintByte(i and 255);
    PrintByte((i shr 8) and 255);
end;

 begin
  textsize:=0;
  textlinesize:=0;
  first := ' ';

  if datafile<>nil then datafile.clear;


  found := 0;
  text  :='// 0000 : u16 : signature  = 0x4659'#13#10+
          '// 0002 : u8  : version   = 0x1'#13#10+
          '// 0003 : u8  : bits per pixel '#13#10+
          '// 0004 : u16 : bitmap width '#13#10+
          '// 0006 : u8  : bitmap height'#13#10+
          '// 0007 : u8  : bitmap baseline'#13#10+
          '// 0008 : u8  : first char'#13#10+
          '// 0009 : u8  : last  char'#13#10+
          '// 000A : u16*char count: character last colunm offset '#13#10+
          '//      : bitmap data'#13#10;

  // detect base line
  setlength(baselineCounter,bm.height);
  for i:=0 to  bm.width-1  do
   begin
     j := 0;
     while (j<bm.height) and (bm.canvas.pixels[i,bm.height-j-1]<>0) do inc(j);
     if (j<bm.height) then inc (baselineCounter[j]);
   end;

   max:=0;
   baseline:=0;
   for i:=0 to bm.height-1 do
     if (baselineCounter[i]>max)  then
       begin
          max   := baselineCounter[i];
          baseline := i;
       end;

  textlinesize:=0;
  textsize:=0;

  for i:=0 to  bm.width-1 do
   if  ((bm.canvas.pixels[i,0] and $FFFFFF) =0) then
     if (found<255-31)  then
       inc(found);


  PrintByte(ord('Y'));
  PrintByte(ord('F'));
  PrintByte(1);
  PrintByte(1);
  PrintInteger(bm.width);
  PrintByte(bm.height-1);
  PrintByte(baseline);

  PrintByte(ord(first));
  PrintByte(31+found);

  found:=0;
  for i:=0 to  bm.width-1 do
   if  ((bm.canvas.pixels[i,0] and $FFFFFF) =0) then
     if (found<255-31)  then
         PrintInteger(i);

  begin
    for j:=1 to bm.height-1 do
     for i:=0 to (bm.width shr 3) -1 do
      begin
        b :=0;
        for k:=0 to 7 do
         begin
          color :=   bm.canvas.pixels[(i shl 3) +k,j];
          if (color   and $FFFFFF =0) then b:=b or (128 shr k);
         end;
        PrintByte(b);
      end;
   end;

  BitmapToYfData:=text;

 end;

procedure YfDataToBitmap(mem:tmemorystream;bm:tbitmap);
 function readbyte():byte;
  var b:byte;
  begin
   mem.read(b,1);
   readbyte:=b;
  end;

 function readinteger():integer;
 var  msb,lsb:byte;
  begin
   mem.read(lsb,1);
   mem.read(msb,1);

   readinteger:=lsb + (256 * msb);
  end;

 var
 width,height,first,last,i,x,y,j,v : integer;
 begin
   mem.seek(0,soFromBeginning);
   readinteger(); // signature
   readbyte();  // version
   readbyte();  // bpx
   width      := (readinteger()+15) and $fffffff0;
   height     := readbyte();
    readbyte(); // baseline
   first      := readbyte();
   last       := readbyte();
   bm.width:=width;
   bm.height:=Height+1;
   bm.canvas.Brush.color:=$ffffff;
   bm.canvas.fillrect(rect(0,0,width,height)) ;

   for i:=first to last do
    begin
     x:=readinteger();
     bm.canvas.pixels[x,0]:=0;
    end;

   x:=0;y:=1;
   while(mem.position<mem.size) do
    begin
     v:=readinteger();
     v := ((v and $ff) shl 8) or (v shr 8);
     for j:=0 to 15 do
     if ((v and (1 shl j))<>0) then bm.canvas.pixels[x+15-j,y]:=0;
     x:=x+16;
     if x>=width then
     begin
      x:=0;
      inc(y);
     end;
    end;



 

 end;


end.
