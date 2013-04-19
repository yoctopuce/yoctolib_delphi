program helloworld;
{$APPTYPE CONSOLE}
uses
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
    halt;
  End;

var
  disp      : TYDisplay;
  l0,l1     : TYDisplayLayer;
  errmsg    : string;
  w,h       : integer;
  x,y,vx,vy :integer;


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

  // display clean up
  disp.resetAll();

  // retreive the display size
  w:=disp.get_displayWidth();
  h:=disp.get_displayHeight();

  // reteive the first layer
  L0:=Disp.get_displaylayer(0);

  // display a text in the middle of the screen
  L0.drawText(w div 2, h div 2, Y_ALIGN_CENTER, 'Hello world!' );
  // visualize eah corner
  L0.moveto(0,5);L0.lineto(0,0);L0.lineto(5,0);
  L0.moveto(0,h-6);L0.lineto(0,H-1);L0.lineto(5,H-1);
  L0.moveto(w-1,h-6);L0.lineto(w-1,H-1);L0.lineto(w-6,H-1);
  L0.moveto(w-1,5);L0.lineto(w-1,0);L0.lineto(w-6,0);

  // draw a circle in the top left corner of layer 1
  L1:=Disp.get_displaylayer(1);
  L1.clear();
  L1.drawCircle(H div 8, H div 8, h div 8);

  // and animate the layer
  Writeln('Use Ctrl-C to stop');
  x:=0; y:=0; vx:=1; vy:=1;
  while (true) do
   begin
    x:=x+vx;y:=y+vy;
    if (x<0) or (x>w-(h div 4)) then vx:=-vx;
    if (y<0) or (y>h-(h div 4)) then vy:=-vy;
    l1.setLayerPosition(x,y,0);
    ysleep(5,errmsg);
   end;

end.
