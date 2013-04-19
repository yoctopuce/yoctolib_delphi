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

var
  disp      : TYDisplay;
  l0        : TYDisplayLayer;
  errmsg    : string;
  w,h,i,j  : integer;
  x,y,x0,y0 : Extended	;
  data      : TBYTEARRAY;
  bytesPerLines,index : integer;
  max_iteration,iteration :integer;
  centerX,centerY,targetX,targetY,zoom,distance: Extended	;
  xtemp : Extended	;

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

  // retreive the display size
  w:=disp.get_displayWidth();
  h:=disp.get_displayHeight();

  //reteive the first layer
  l0 := disp.get_displayLayer(0);

  bytesPerLines := w div 8;

  // allocate buffer size to  display size
  setlength(data, h * bytesPerLines);

  // zoom coodinates
  targetX       := 8.34555980181972E-0001;
  targetY       := 2.04552998862566E-0001;




  max_iteration := 50;
  zoom          := 1;
  distance      := 1;


  // mandelbroot calculus
   while (true) do
     begin
      fillchar(data[0],h * bytesPerLines,0);
      distance := distance *0.95;
      centerX :=  targetX * (1-distance);
      centerY :=  targetY * (1-distance);
      max_iteration := round(max_iteration  + sqrt(zoom) );
      if (max_iteration>1500) then max_iteration := 1500;

      for j:=0 to h-1 do
        for i:=0 to w-1 do
          begin

            x0 := (((i - w/2.0) / (w/8))/zoom)-centerX;
            y0 := (((j - h/2.0) / (w/8))/zoom)-centerY;

            x := 0;
            y := 0;

            iteration := 0;

            while ( x*x + y*y < 4)  AND  (iteration < max_iteration ) do
               begin
                xtemp := x*x - y*y + x0;
                y := 2*x*y + y0;
                x := xtemp;
                iteration := iteration + 1;
               end;

            if (iteration>=max_iteration) then
             begin
              // putpixel in data buffer
              index:=  j*bytesPerLines + (i shr 3);
              data[index]:=data[index] or  (128 shr (i mod 8));
             end;

          end;
          
       // send buffer to display
       l0.drawBitmap(0,0,w,data,0);
       zoom :=zoom / 0.95;
   end;
end.
