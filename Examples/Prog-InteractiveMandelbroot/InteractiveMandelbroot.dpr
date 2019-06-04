{*
 * This demo allows to navigate into an Mandelbroot set,
 * You need
 * A Display
 * 2 anButton connected to swiches  called b1 and b2 (Zoom In/Out)
 * 2 anButton connected to swiches called by and by (Pan Hrt/Vrt)
 *}

program helloworld;
{$APPTYPE CONSOLE}
uses
  windows,
  SysUtils,
  yocto_api,
  yocto_display,
  yocto_anbutton;

var
  zoomInPressed,zoomOutPressed : boolean;
  vx,vy : double;


Procedure b1change(b :TYAnButton; value:string);
  begin

    writeln('b1('+value+')');
    zoomInPressed:=strtoint(value)<500;

  end;

Procedure b2change(b :TYAnButton; value:string);
  begin

    writeln('b2('+value+')');
    zoomOutPressed:=strtoint(value)<500;
  end;

Procedure bxchange(b :TYAnButton; value:string);
  var
   v : integer;
  begin
    writeln('bx('+value+')');
    v:= round((strtoint(value)-500)/50);

    vx:= v / 50;
  end;

Procedure bychange(b :TYAnButton; value:string);
  var
   v : integer;
  begin
    writeln('by('+value+')');
    v:= round((strtoint(value)-500)/50);

    vy:= v / 50;
  end;


var
  disp           : TYDisplay;
  l0             : TYDisplayLayer;
  b1,b2,bx,By    : TYanbutton;
  errmsg    : string;
  w,h,i,j  : integer;
  x,y,x0,y0 : Extended	;
  centerX,centerY : extended;
  data      : TBYTEARRAY;
  bytesPerLines,index : integer;
  max_iteration,iteration :integer;
  zoom: Extended	;
  xtemp : Extended	;
  sx,sy,svx,svy,sz  : string;
begin


  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;


   disp:= YFirstDisplay();
   b1:= YfindAnButton('b1');
   b2:= YfindAnButton('b2');
   bx:= YfindAnButton('x');
   by:= YfindAnButton('y');

   if not (b1.isonline()) then
     begin
       writeln('No an button named b1');
       halt;
     end;

   if not (b2.isonline()) then
     begin
       writeln('No an button named b2');
       halt;
     end;

     if not (bx.isonline()) then
     begin
       writeln('No an button named x');
       halt;
     end;

   if not (by.isonline()) then
     begin
       writeln('No an button named y');
       halt;
     end;

   if disp=nil then
     begin
       writeln('No module connected (check USB cable)');
       halt;
     end;


  b1.registerValueCallback(@b1change);
  b2.registerValueCallback(@b2change);

  by.registerValueCallback(@bychange);
  bx.registerValueCallback(@bxchange);



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
  zoomInPressed:=false;
  zoomOutPressed:=false;
  vx:=0;
  vy:=0;
  centerX :=0;
  centerY :=0;

  max_iteration := 50;
  zoom          := 1;


  // mandelbroot calculus
   while (true) do
     begin
     
      fillchar(data[0],h * bytesPerLines,0);
      if zoomInPressed  then zoom:=zoom*1.1;
      if zoomoutPressed  then zoom:=zoom/1.1;

      centerX := centerX - (vx / zoom);
      centerY := centerY + (vy / zoom);


      max_iteration := round(max_iteration  + sqrt(zoom) );
      if (max_iteration>1500) then max_iteration := 1500;

      for j:=0 to h-1 do
        begin
        // yHandleEvents(errmsg);
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
        end;
       // send buffer to display
       l0.drawBitmap(0,0,w,data,0);
       yHandleEvents(errmsg);

   end;
end.
