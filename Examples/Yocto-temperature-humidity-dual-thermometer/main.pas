

unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  yocto_api,yocto_temperature, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    sensor1: TGroupBox;
    temp1: TLabel;
    beacon1: TCheckBox;
    GroupBox1: TGroupBox;
    temp2: TLabel;
    beacon2: TCheckBox;
    graph: TImage;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    StatusBar1: TStatusBar;
    procedure Timer1Timer(Sender: TObject);
    procedure beacon1Click(Sender: TObject);
    procedure beacon2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function getTemp(serial:string):double;
    function getBeacon(serial:string):boolean;
    procedure toggleBeacon(serial:string);
    Procedure populateComboBox(c:TcomboBox;b:tcheckbox);

  public
    { Public declarations }
    procedure refreshComboBoxes();

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const scale = 7;
procedure devicelistchanged(m:tYmodule);

 begin
   // something has changed in the devices list
   // lets refresh it, quick and dirty way.
   form1.refreshComboBoxes();
 end;

function TForm1.GetTemp(serial:string):double;
 var
  sensor:tytemperature;

 begin
  sensor:=yFindTemperature(serial+'.temperature');
  if (sensor.isOnline()) then
      GetTemp:=sensor.get_currentValue()
    else GetTemp:=-999;
 end;


function Tform1.getBeacon(serial:string):boolean;

 var
   module : Tymodule;
   res    : boolean;
 begin
   res:=false;
   module:=yFindModule(serial);
   if module.isOnline() then
     if (module.get_beacon()=Y_BEACON_ON) then   res:=true;
   GetBeacon:=res;
 end;

procedure Tform1.toggleBeacon(serial:string);
var
   module : Tymodule;
   beacon :boolean;
 begin
   beacon := GetBeacon(serial);
   module:=yFindModule(serial);
   if module.isOnline() then
    if beacon   then module.set_beacon(Y_BEACON_OFF)
                else  module.set_beacon(Y_BEACON_ON);

 end;


Procedure Tform1.populateComboBox(c:TcomboBox;b:tcheckbox);
 var
   m:TyModule;
   sensor :Tytemperature;
   selected,serial : string;
   i,index:integer;
 begin
  selected :='';
  if (c.itemindex>0) then selected  := c.items[c.itemindex];
  sensor:=yFirstTemperature();
  c.clear;
  while (sensor<>nil) do
   begin
    m:= sensor.get_module();
    serial :=  m.get_serialnumber();
    c.items.add(serial);
    sensor:=sensor.nextTemperature();
   end;
   if (c.items.count<=0) then c.enabled:=false
    else
    begin
     c.enabled:=true;
     index:=0;
     for i:=0 to c.items.count-1 do
       if (c.items[i]=selected) then   index:=i;
     c.itemindex:=index;

   end;
   b.enabled:=c.enabled;

   if c.items.count =0 then  statusbar1.panels[0].text:='Plug any Yocto-device feature a temperature sensor'
   else if c.items.count =1 then  statusbar1.panels[0].text:='One device connected'
   else StatusBar1.panels[0].text:=intToStr(c.items.count)+' devices connected';

 end;



procedure TForm1.Timer1Timer(Sender: TObject);
 var t1,t2 : double;
  r1,r2    : trect;
  y,i      : integer;
  errmsg   : string;
begin
  yUpdateDeviceList(errmsg); // scan for changes

  r1 := Rect(1,1,graph.width-3,graph.height-2);
  r2 := Rect(2,1,graph.width-2,graph.height-2);
  graph.canvas.CopyRect(r1, graph.canvas  , r2);

  for i:=-2 to trunc(graph.height / scale) -2  do
   begin
     if (i=0) then  graph.canvas.pen.color:=clBlack
               else  graph.canvas.pen.color:=clGray;
     graph.Canvas.MoveTo(1, graph.height-5-((i*10)+20)*scale);
     graph.Canvas.LineTo(graph.width-2, graph.height-5-((i*10)+20)*scale );

   end;


  if (combobox1.enabled) then
   begin
    t1 :=    GetTemp(combobox1.items[combobox1.itemIndex]);
    temp1.caption := floattostr(t1) + ' °C' ;
    graph.canvas.pen.color:=clgreen;
    y :=  round(graph.height-5-(t1+20) *scale);
    graph.canvas.moveto(graph.width-4,y );
    graph.canvas.lineto(graph.width-4,y+1 );
    beacon1.onclick:=nil;
    beacon1.checked:=getBeacon(combobox1.items[combobox1.itemIndex]);
     beacon1.onclick:=beacon1Click;
   end else temp1.caption := 'N/A';

  if (combobox2.enabled) then
   begin
    t2 :=    GetTemp(combobox2.items[combobox2.itemIndex]);
    temp2.caption := floattostr(t2) + ' °C' ;
    graph.canvas.pen.color:=clblue;
    y :=  round(graph.height-5-(t2+20) *scale);
    graph.canvas.moveto(graph.width-4,y );
    graph.canvas.lineto(graph.width-4,y+1 );
    beacon2.onclick:=nil;
    beacon2.checked:=getBeacon(combobox2.items[combobox2.itemIndex]);
    beacon2.onclick:=beacon2Click;
   end else temp2.caption := 'N/A';


end;

procedure TForm1.beacon1Click(Sender: TObject);
begin
  if (combobox1.enabled)then toggleBeacon(combobox1.items[combobox1.itemIndex]);

end;

procedure TForm1.beacon2Click(Sender: TObject);
begin
  if (combobox2.enabled)then toggleBeacon(combobox1.items[combobox2.itemIndex]);
end;




procedure TForm1.FormCreate(Sender: TObject);
var
 errmsg:string;
 i:integer;
begin
  errmsg := '';
  graph.canvas.brush.style := bsSolid;
  graph.canvas.brush.color := clwhite;
  graph.canvas.rectangle(0,0, graph.width-1,graph.height-1);

   for i:=-2 to trunc(graph.height / scale) -2  do
   begin
     if (i=0) then  graph.canvas.pen.color:=clBlack
               else  graph.canvas.pen.color:=clGray;
     graph.Canvas.MoveTo(1, graph.height-5-((i*10)+20)*scale);
     graph.Canvas.LineTo(graph.width-2, graph.height-5-((i*10)+20)*scale );

   end;

  yRegisterDeviceArrivalCallBack( devicelistchanged);
  yRegisterDeviceRemovalCallBack( devicelistchanged);

end;

procedure  Tform1.refreshComboBoxes();
 begin
  populateComboBox(combobox1,beacon1);
  populateComboBox(combobox2,beacon2);
 end;

var errmsg : string;

initialization

  yRegisterhub('usb',errmsg);
  yDisableExceptions();

end.
