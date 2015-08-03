program helloworld;
{$APPTYPE CONSOLE}
uses
  windows,
  SysUtils,
  yocto_api,
  yocto_display;


Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any');
    sleep(3000);
    halt;
  End;

// this is the recusive function to draw 1/3nd of the Von Koch flake
procedure recursiveLine(layer:TYDisplaylayer; x0,y0,x1,y1 :double; deep :integer);
  var
    dx,dy,mx,my : double;
  begin
    if (deep<=0) then
      begin
        layer.moveto(round(x0),round(y0));
        layer.lineto(round(x1),round(y1));
      end
      else
      begin
        dx := (x1-x0) /3;
        dy := (y1-y0) /3;
        mx :=  ((x0+x1) / 2) +  (0.87 *(y1-y0) / 3);
        my :=  ((y0+y1) / 2) -  (0.87 *(x1-x0) / 3);
        recursiveLine(layer,x0,y0,x0+dx,y0+dy,deep-1);
        recursiveLine(layer,x0+dx,y0+dy,mx,my,deep-1);
        recursiveLine(layer,mx,my,x1-dx,y1-dy,deep-1);
        recursiveLine(layer,x1-dx,y1-dy,x1,y1,deep-1);
      end;
  end;

var
  disp              : TYDisplay;
  l1                : TYDisplayLayer;
  errmsg            : string;
  centerX,CenterY   : double;
  radius,a          : double;
  i                 : integer;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  // first one of the two RBG leds
  if paramstr(1)='any' then
    begin
      disp := yFirstDisplay();
      if disp=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
      end
   else
  disp:= YFindDisplay(paramstr(1)+'.display');

  // make sure it is online
  if not(disp.isOnline()) then
      begin
        writeln('No module connected (check USB cable)');
        halt;
      end;

  writeln('Hit Ctrl-C to stop');
      
  // display clean up
  disp.resetAll();

  l1 := disp.get_displayLayer(1);
  l1.hide();    // L1 is hidden, l2 stay visible
  centerX := disp.get_displayWidth() / 2;
  centerY := disp.get_displayHeight() / 2;
  radius  := disp.get_displayHeight() / 2;
  a:=0;

  while (true) do
    begin
     // we draw in the hidden layer
     l1.clear();
     for i:=0 to 2 do
        recursiveLine(l1,centerX + radius*cos(a+i*2.094),
                         centerY + radius*sin(a+i*2.094) ,
                         centerX + radius*cos(a+(i+1)*2.094),
                         centerY + radius*sin(a+(i+1)*2.094), 2);
     // then we swap contents with the visible layer

     disp.swapLayerContent(1,2);
     // change the flake angle
     a:=a+0.1257;
   end;
end.

