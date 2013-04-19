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

const
  count = 8;

var
  disp      : TYDisplay;
  l0        : TYDisplayLayer;
  errmsg    : string;
  coord      : array[0..(count*2)] of integer;
  ledwidth : integer;
  w,h,i,framesCount       : integer;
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

  //reteive the first layer
  l0 := disp.get_displayLayer(0);

  // precompute the "leds" position
  ledwidth:= (w div count);
  for i:=0 to count-1 do
    begin
     coord[i] := i *ledwidth;
     coord[2*count-i-2] := i *ledwidth ;
    end;
  framesCount :=  2*count-2;

  // start recording
  disp.newSequence();

  // build one loop for recording
  for i:=0 to framesCount-1 do
     begin
       l0.selectColorPen(0);
       l0.drawbar(coord[(i+framesCount-1) mod framesCount], h-1,coord[(i+framesCount-1) mod framesCount]+ledwidth, h-4);
       l0.selectColorPen($ffffff);
       l0.drawbar(coord[i], h-1, coord[i]+ledwidth, h-4);
       disp.pauseSequence(50);  // records a 50ms pause.
     end;
  // self-call : causes an endless looop
  disp.playSequence('K2000.seq');
  // stop recording and save to device filesystem
  disp.saveSequence('K2000.seq');

  // play the sequence
  disp.playSequence('K2000.seq');

  Writeln('This animation is running in background.');

end.
