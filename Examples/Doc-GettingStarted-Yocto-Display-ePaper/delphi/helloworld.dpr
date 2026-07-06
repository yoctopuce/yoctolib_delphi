{*********************************************************************
 *
 *  $Id: helloworld.dpr 75054 2026-07-02 10:01:23Z seb $
 *
 *  An example that shows how to use a  Yocto-Display-ePaper
 *
 *  You can find more information on our web site:
 *   Yocto-Display-ePaper documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-display-epaper/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  {$IFNDEF UNIX}
  windows,
  {$ENDIF UNIX} 
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

var
  disp            : TYDisplay;
  l0              : TYDisplayLayer;
  errmsg          : string;
  w,h             : integer;
  paneltype       : string;
  middleX,middleY : integer;
  iterations      : integer;
  animation       : boolean;
  cx,cy,r,i       : integer;

const
  colors: array[0..3] of integer =( $FFFFFF, $000000, $FF0000, $FFFF00 );

begin
  errmsg:='';
  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    sleep(3000);
    exit;
  end;

  // first one of the two RBG leds
  if paramstr(1)='any' then
    begin
      disp := yFirstDisplay();
      if disp=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
      end
   else
  disp:= YFindDisplay(paramstr(1)+'.display');

  // make sure it is online
  if not(disp.isOnline()) then
      begin
        writeln('No module connected (check USB cable)');
        sleep(3000);
        halt;
      end;

  // Makes sure the Panel type is set
  paneltype := disp.get_displayPanel();
  if (paneltype = 'NOT_SET') then
    begin
       Writeln('Use the virtual to Configure the panel first');
       ySleep(3000, errmsg);
       halt;
     end;

  // retrieve the display size
  w := disp.get_displayWidth();
  h := disp.get_displayHeight();
  middleX := w div 2;
  middleY := h div 2;
  writeln( 'Using device ' +disp.get_serialNumber() +' (panel: ' + paneltype + ' ' + IntToStr(w) + 'x' + IntToStr(h) + 'pixels)');
  disp.resetAll();

  // retrieve the first layer
  l0 := disp.get_displayLayer(0);
  l0.selectFont('medium.yfm');
  iterations := 0;
  animation := true;

  while (animation) do
    begin
     iterations:=iterations+1;
     // prevent refreshing for 2 sec
     disp.postponeRefresh(2000);
     l0.clear();
     // draw a few circle
     for  i := 1 to 15   do
      begin
         cx := trunc(Random * w);
         cy := trunc(Random  * h);
         r  := trunc(h / 20) + trunc(Random * h / 10);
         l0.selectFillColor(colors[trunc(4*Random)]);
         l0.drawDisc(cx, cy, r);
         l0.drawCircle(cx, cy, r);
      end;
    // draw a rectangle with panel type in it
    l0.selectFillColor($ffffff);
    l0.drawBar(middleX - 75, middleY - 10, middleX + 75, middleY + 12);
    l0.drawRect(middleX - 75, middleY - 10, middleX + 75, middleY + 12);
    l0.drawText(middleX, middleY, Y_ALIGN_CENTER, paneltype);
    // forces a full refresh only the 1rst time
    if (iterations = 1) then disp.regenerateDisplay();
    disp.triggerRefresh(); // display is allowed to refresh  again
    ySleep(1000, errmsg);
    // if no fast refresh available, don't even try to run animations
    if (pos('KS',paneltype)<=0) then  animation := false;
  end;
  yFreeAPI();
end.
