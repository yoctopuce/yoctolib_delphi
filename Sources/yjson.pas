{*********************************************************************
 *
 * $Id: yjson.pas 67454 2025-06-13 10:17:04Z seb $
 *
 * Simple JSON parser to parse the output of Yoctopuce devices
 *
 * - - - - - - - - - License information: - - - - - - - - -
 *
 *  Copyright (C) 2011 and beyond by Yoctopuce Sarl, Switzerland.
 *
 *  Yoctopuce Sarl (hereafter Licensor) grants to you a perpetual
 *  non-exclusive license to use, modify, copy and integrate this
 *  file into your software for the sole purpose of interfacing
 *  with Yoctopuce products.
 *
 *  You may reproduce and distribute copies of this file in
 *  source or object form, as long as the sole purpose of this
 *  code is to interface with Yoctopuce products. You must retain
 *  this notice in the distributed source file.
 *
 *  You should refer to Yoctopuce General Terms and Conditions
 *  for additional information regarding your rights and
 *  obligations.
 *
 *  THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT
 *  WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 *  WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS
 *  FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO
 *  EVENT SHALL LICENSOR BE LIABLE FOR ANY INCIDENTAL, SPECIAL,
 *  INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA,
 *  COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR
 *  SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT
 *  LIMITED TO ANY DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR
 *  CONTRIBUTION, OR OTHER SIMILAR COSTS, WHETHER ASSERTED ON THE
 *  BASIS OF CONTRACT, TORT (INCLUDING NEGLIGENCE), BREACH OF
 *  WARRANTY, OR OTHERWISE.
 *
 *********************************************************************}
unit yjson;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses sysutils,classes;

const
  JSONGRANULARITY = 10;

type
  TStringArrayJson =  array of string;
  TByteArrayJson =  array of byte;
  TByteArrayArrayJson =  array of TByteArrayJson;
  PJSONRECORD  = ^TJSONRECORD;
  PASONRECORD  = ^ASONRECORD;
  APJSONRECORD =  array of PJSONRECORD;
  TJSONRECORDTYPE = (JSON_STRING,JSON_INTEGER,JSON_BOOLEAN,JSON_STRUCT,JSON_ARRAY);
  ASONRECORD   = array[0..4095] of PJSONRECORD;     // the 0..4095 means nothing, the stuff is dynamicaly allocated
  TJSONRECORD = record
   name : string [64];
   case recordtype :  TJSONRECORDTYPE OF
      JSON_STRING  : (svalue  : pansichar);
     JSON_INTEGER  : (ivalue  : int64);
     JSON_BOOLEAN  : (bvalue  : boolean);
      JSON_STRUCT  : (membercount     : integer;
                      memberAllocated : integer;
                      members         : PASONRECORD);
      JSON_ARRAY   : (itemcount       : integer;
                      itemAllocated   : integer;
                      items           : PASONRECORD);


  end;

  type
   Tjstate = (JSTART,JWAITFORNAME,JWAITFORENDOFNAME,JWAITFORCOLON,JWAITFORDATA,JWAITFORNEXTSTRUCTMEMBER,
             JWAITFORNEXTARRAYITEM, JSCOMPLETED,JWAITFORSTRINGVALUE,JWAITFORINTVALUE,JWAITFORBOOLVALUE);


  Tjsonlogfct = procedure(msg:string) of object;

  TJsonParser = class(tobject)

  private


   data     : PJSONRECORD;
   httpMsg  : string;
   logfct   : Tjsonlogfct;
   function  Parse(st:string): PJSONRECORD;
   procedure FreeStructure(p:PJSONRECORD);
   function  createStrRecord(name,value:string):PJSONRECORD;
   function  createIntrecord(name:string;value:int64):PJSONRECORD;
   function  createBoolRecord(name:string;value:boolean):PJSONRECORD;
   function  createStructRecord(name:string):PJSONRECORD;
   procedure add2StructRecord(container:PJSONRECORD;element:PJSONRECORD);
   Procedure ParseError(var st:string; i:integer; errmsg:string);
   function  ParseEx(initialstate:Tjstate;defaultname:string;var st:string;var i:integer):PJSONRECORD;
   procedure DumpStructureRec(p:PJSONRECORD; deep:integer; logfct: Tjsonlogfct);
   function  createArrayRecord(name:string):PJSONRECORD;
   procedure add2ArrayRecord(container:PJSONRECORD;item:PJSONRECORD);

  public
   httpcode : integer;
   constructor create(jsonData:string);  overload;
   constructor create(jsonData:string; withHTTPHeader : boolean);   overload;
   function    GetHTTPcode(var msg:string):integer;
   function    GetChildNode(parent:PJSONRECORD; nodename:string):PJSONRECORD;
   function    GetAllChilds(parent:PJSONRECORD):TByteArrayArrayJson;
   function    GetRootNode():PJSONRECORD;
   procedure   SetLogFunction(fct:Tjsonlogfct);
   procedure   DumpStructure( logfct: Tjsonlogfct);
   destructor  Destroy(); override;
   function    getHTTcode():integer;
   function    convertToString(p:PJSONRECORD; showNamePrefix:boolean):string;
   function    convertToBytes(p:PJSONRECORD; showNamePrefix :boolean):TByteArrayJson;
  end;





implementation


  function TJsonParser.convertToBytes(p:PJSONRECORD; showNamePrefix :boolean):  TByteArrayJson;
    var
      value :string;
      i: integer;
      res: TByteArrayJson;
    begin
      value := self.convertToString(p, showNamePrefix);
      SetLength(res, length(value));
      for i := 0 to Length(value) - 1 do
        res[i] := ord(value[i + 1]);
      result := res;
    end;


function  TJsonParser.convertToString(p:PJSONRECORD; showNamePrefix :boolean):string;
 var
   buffer: string;
   i :integer;
 begin

  if (p=nil) then p:=data;
  if  (p^.name<>'') and showNamePrefix then buffer:= '"'+string(p^.name)+'":' else buffer:='';
  case p^.recordtype of
      JSON_STRING  : buffer:= buffer+'"'+string(p^.svalue)+'"';
      JSON_INTEGER : buffer:= buffer+ inttostr(p^.ivalue);
      JSON_BOOLEAN : if (p^.bvalue) then  buffer:= buffer+ 'TRUE'
                                    else  buffer:= buffer+'FALSE';
      JSON_STRUCT :  begin
                       buffer:= buffer+'{';
                       for i:=0 to p^.membercount-1 do
                         begin
                           if (i>0) then buffer:=buffer+',';
                           buffer:=buffer+self.convertToString(p^.members^[i],true);
                         end;
                       buffer:= buffer+'}';
                     end;
      JSON_ARRAY :   begin
                     buffer:= buffer+'[';
                     for i:=0 to p^.itemcount-1 do
                       begin
                         if (i>0) then buffer:=buffer+',';
                         buffer:=buffer+self.convertToString(p^.members^[i],false);
                       end;
                     buffer:= buffer+']';
                    end;
  end;
  convertToString := buffer;
 end;

function  TJsonParser.GetRootNode():PJSONRECORD;
 begin
   GetRootNode:=data;
 end;


procedure   TJsonParser.SetLogFunction(fct:Tjsonlogfct);
 begin
   logfct:=fct;
 end;

function  TJsonParser.getHTTcode():integer;
 begin
 getHTTcode := httpcode;
 end;

constructor  TJsonParser.create(jsonData:string);
begin
  self.create(jsonData,true);
end;




constructor  TJsonParser.create(jsonData:string;withHTTPHeader : boolean);
 const
   httpheader= 'HTTP/1.1 ';
   okHeader='OK'#13#10;
 var
   errmsg: string;
   p1,p2 : integer;
   start_Struct,start_array : integer;
 begin
  httpcode  := 0;
  if withHTTPHeader then
    begin
     if (copy(jsonData,1,length(okHeader))=okHeader) then
      begin
       httpcode:=200;
      httpmsg:='OK';
      end
      else
      begin
        if (copy(jsonData,1,length(httpheader))<>httpheader)  then
          begin
           errmsg := 'data should start with '+httpheader;
           raise Exception.Create(errmsg);
          end;

       p1:=length(httpheader)+pos(' ',copy(jsonData,length(httpheader)+1,length(jsonData)-length(httpheader)));
       p2:=pos(#13#10,jsonData);

       httpcode  :=  StrToInt(copy(jsonData,length(httpheader),p1-length(httpheader)));

       httpmsg:= copy(jsonData,p1,p2-p1);
       if  (httpcode<>200)  then  exit;
     end;
     p1:= pos(#13#10#13#10'{',jsonData);       // json data is a structure
     if (p1<=0) then p1:= pos(#13#10#13#10'[',jsonData);   // json data is an array
     if (p1<=0) then
     begin
       errmsg := 'data  does not contain JSON data ';
       raise Exception.Create(errmsg);
      end;

     jsonData := copy(jsonData,p1+4,length(jsonData)-p1-3);
    end
    else
    begin
      start_struct :=  pos('{',jsonData); // json data is a structure
      start_array  :=  pos('[',jsonData); //  json data is an array
      if (start_array < 0) and (start_struct < 0)  then
       begin
           errmsg := 'data  does not contain JSON data';
           raise Exception.Create(errmsg);
       end;
    end;

   data:=parse(jsonData);

 end;

destructor  TJsonParser.destroy();
 begin
  if assigned(data) then FreeStructure(data);
  data:=nil;
  inherited destroy();
 end;

function  TJsonParser.GetHTTPcode(var msg:string):integer;
 begin
  msg   := httpmsg;
  result:= httpcode;
 end;

function  TJsonParser.createStrRecord(name,value:string):PJSONRECORD;
 var
   res : PJSONRECORD;
   avalue : ansistring;
   l: integer;
 begin
   getmem(res,sizeof(TJSONRECORD)) ;
   res^.name       :=  shortstring(name);
   res^.recordtype :=  JSON_STRING;
   l               :=  length(value);
   getmem(res^.svalue,l+1);
   avalue := ansistring(value);
   if l>0 then move(avalue[1],res^.svalue^,l);
   res^.svalue[l] := #0;
   createStrRecord := res;
 end;

function  TJsonParser.createIntrecord(name:string;value:int64):PJSONRECORD;
 var res : PJSONRECORD;

 begin
   getmem(res,sizeof(TJSONRECORD)) ;
   res^.name       :=  shortstring(name);
   res^.recordtype :=  JSON_INTEGER;
   res^.ivalue     := value;
   createIntrecord := res;
 end;

function  TJsonParser.createBoolRecord(name:string;value:boolean):PJSONRECORD;
 var res : PJSONRECORD;

 begin
   getmem(res,sizeof(TJSONRECORD)) ;
   res^.name       :=  shortstring(name);
   res^.recordtype :=  JSON_BOOLEAN;
   res^.bvalue     :=  value;
  createBoolRecord := res;
 end;

function TJsonParser.createStructRecord(name:string):PJSONRECORD;
 var res : PJSONRECORD;

 begin
   getmem(res,sizeof(TJSONRECORD)) ;
   res^.name       :=  shortstring(name);
   res^.recordtype :=  JSON_STRUCT;
   res^.membercount:=0;
   res^.memberAllocated:=JSONGRANULARITY;
   GetMem(res^.members, sizeof(PJSONRECORD)*res^.memberAllocated);
   createStructRecord := res;
 end;

procedure TJsonParser.add2StructRecord(container:PJSONRECORD;element:PJSONRECORD);
 var
   p: PASONRECORD;
 begin
   if (container^.recordtype <> JSON_STRUCT) then
      raise Exception.Create('container is not struct type');

   if (container^.membercount>=container^.memberAllocated)  then
    begin
      GetMem(p,(container^.memberAllocated+JSONGRANULARITY) * sizeof(PJSONRECORD));
      move(container^.members^,p^, container^.memberAllocated * sizeof(PJSONRECORD));
      freemem(container^.members);
      container^.members  := p ;
      inc(container^.memberAllocated,JSONGRANULARITY);
    end;

   container^.members^[container^.membercount] := element;
   inc(container^.membercount);

 end;




function TJsonParser.createArrayRecord(name:string):PJSONRECORD;
 var res : PJSONRECORD;

 begin
   getmem(res,sizeof(TJSONRECORD)) ;
   res^.name       :=  shortstring(name);
   res^.recordtype :=  JSON_ARRAY;
   res^.itemcount:=0;
   res^.itemAllocated:=JSONGRANULARITY;
   GetMem(res^.items, sizeof(PJSONRECORD)*res^.itemAllocated);
   createArrayRecord := res;
 end;

procedure TJsonParser.add2ArrayRecord(container:PJSONRECORD;item:PJSONRECORD);
 var
   p: PASONRECORD;
 begin
   if (container^.recordtype <> JSON_ARRAY) then
      raise Exception.Create('container is not array type');

   if (container^.itemcount>=container^.itemAllocated)  then
    begin
      GetMem(p,(container^.itemAllocated+JSONGRANULARITY) * sizeof(PJSONRECORD));
      move(container^.items^,p^, container^.itemAllocated * sizeof(PJSONRECORD));
      freemem(container^.items);
      container^.items  := p ;
      inc(container^.itemAllocated,JSONGRANULARITY);
    end;

   container^.items^[container^.itemcount] := item;
   inc(container^.itemcount);
 end;





Procedure TJsonParser.ParseError(var st:string; i:integer; errmsg:string);
 var
  ststart,stend:integer;
 begin
  ststart := i-10;
  stend   := i+10;
  if (ststart<1) then ststart := 1;
  if (stend>length(st)) then  stend := length(st);
  errmsg := errmsg+' near '+copy(st,ststart,i-1)+'*'+copy(st,i,stend-i);
  raise Exception.Create(errmsg);
 end;



function TJsonParser.ParseEx(initialstate:Tjstate;defaultname:string; var st:string;var i:integer):PJSONRECORD;
 var

  res,value   : PJSONRECORD;
  state:Tjstate;
  svalue:string;
  //current  :string;
  ivalue:int64;
  isign,v:integer;
  c :char;
  name:string ;

 // procedure DBG(); begin  current :=copy(st,1,i-1)+'>'+st[i]+'<'+copy(st,i+1,length(st)-i);end;
 begin
  name  := defaultname;
  state := initialstate;
  isign := 1;
  res   := nil;
  ivalue:= 0;
  while (i<=length(st)) do
   begin
    //DBG();
    case  state of
     JWAITFORNAME :
               begin
               if (st[i]='"') then  state := JWAITFORENDOFNAME
                 else if ((st[i]<>#32) and (st[i]<>#13) and (st[i]<>#10))  then ParseError(st,i,'invalid char: var expecting "');
               end;
    JWAITFORENDOFNAME :
               begin
               if (st[i]='"') then  state := JWAITFORCOLON
               else if (ord(st[i])>=32) then name:=name+st[i]
               else  ParseError(st,i,'invalid char: was expecting an identifier compliant char');
               end;
   JWAITFORCOLON :
               begin
               if (st[i]=':') then  state := JWAITFORDATA
               else if ((st[i]<>#32) and (st[i]<>#13) and (st[i]<>#10)) then ParseError(st,i,'invalid char: var expecting :');
               end;
     JWAITFORDATA:
               begin
                 if (st[i]='{') then
                  begin
                     res   :=  createStructRecord(name);
                     state :=  JWAITFORNEXTSTRUCTMEMBER;
                  end
                else
                 if (st[i]='[') then
                  begin
                    res   :=  createArrayRecord(name);
                    state :=  JWAITFORNEXTARRAYITEM;

                  end
                else
                 if (st[i]='"') then begin svalue := ''; state := JWAITFORSTRINGVALUE;end
                 else if ((st[i]>='0') AND (st[i]<='9') ) then begin state :=  JWAITFORINTVALUE; ivalue := ord(st[i])-48;isign:=1; end
                 else if ((st[i]='-') ) then begin state :=  JWAITFORINTVALUE; ivalue := 0; isign:=-1; end
                 else if ((upcase(st[i])='T') or (upcase(st[i])='F')) then  begin svalue := upcase(st[i]);state := JWAITFORBOOLVALUE; end
                 else if ((st[i]<>#32) and (st[i]<>#13) and (st[i]<>#10)) then ParseError(st,i,'invalid char: var expecting  ",0..9,t or f');
                 end;
    JWAITFORSTRINGVALUE :
                 begin
                  if (st[i]='\') and (i<length(st)) then
                   begin
                     svalue := svalue +st[i+1];
                     inc(i);
                   end
                  else
                  if (st[i]='"') then
                   begin
                    state :=  JSCOMPLETED;
                    res   :=  createStrRecord(name,svalue);
                   end
                   else if (st[i]<#32)  then ParseError(st,i,'invalid char: was expecting string value')
                   else svalue := svalue +st[i];
                  end;
     JWAITFORINTVALUE:
                  begin
                   c := st[i];
                   if ((c>='0') AND (c<='9'))
                    then
                     begin
                      v := ord(c);
                      ivalue := (ivalue *10) + v-48;
                    end
                   else
                    begin
                      res  :=  createIntRecord(name,isign*ivalue);
                      state :=  JSCOMPLETED;
                      dec(i);
                    end;
                  end;
     JWAITFORBOOLVALUE:
                  begin
                    c :=  upcase(st[i]);
                    if ((c<'A') OR (c>'Z'))  then
                     begin
                      if ((svalue<>'TRUE') AND (svalue<>'FALSE')) then  ParseError(st,i,'unexpected value, was expecting "true" or "false"');
                      res   :=  createBoolRecord(name,svalue='TRUE');
                      state :=  JSCOMPLETED;
                      dec(i);
                     end
                    else svalue:=svalue+c;
                   end;
    JWAITFORNEXTSTRUCTMEMBER :
                begin
                 while ((i<=length(st)) and ((st[i]=#32) or (st[i]=#13) or (st[i]=#10))) do inc(i);
                 if (i<length(st)) then
                  begin
                   if (st[i]='}') then
                    begin
                     ParseEx := res;
                     inc(i);
                     exit;
                    end
                   else
                   begin
                     value :=  ParseEx(JWAITFORNAME,'',st,i);  // DBG();
                     add2StructRecord(res,value);
                     while ((i<=length(st)) and ((st[i]=#32) or (st[i]=#13) or (st[i]=#10))) do inc(i);
                     if ((i<=length(st)) and (st[i]='}')) then dec(i)
                     else if ((i>length(st)) or  (st[i]<>',')) then ParseError(st,i,'invalid char: var expecting , or }');
                   end;
                 end;
                end;
    JWAITFORNEXTARRAYITEM :
                begin
                while ((i<=length(st)) and ((st[i]=#32) or (st[i]=#13) or (st[i]=#10))) do inc(i);
                if (i<length(st)) then
                  begin
                   if (st[i]=']') then
                    begin
                     ParseEx := res;
                     inc(i);
                     exit;
                    end
                   else
                   begin
                     value :=  ParseEx(JWAITFORDATA,inttostr(res.itemcount),st,i);  // DBG();
                     add2ArrayRecord(res,value);
                     while ((i<=length(st)) and ((st[i]=#32) or (st[i]=#13) or (st[i]=#10))) do inc(i);
                     if ((i<=length(st)) and (st[i]=']')) then dec(i)
                     else if ((i>length(st)) or  (st[i]<>',')) then ParseError(st,i,'invalid char: var expecting , or ]');
                   end;
                 end;
                end;

    JSCOMPLETED : begin
                  ParseEx := res;
                  exit;
                 end;

     end;
     inc(i);
   end;
   ParseError(st,i,'expected end of');
   ParseEx := nil;
 end;

procedure TJsonParser.DumpStructureRec(p:PJSONRECORD; deep:integer; logfct: Tjsonlogfct);
 var
  line,indent : string;
  i    : integer;
 begin

   line :='';
   indent := '' ;
   for i:=0 to deep*2 do indent:=indent+#32;
   line:=indent+string(p^.name) + ':';
   case p^.recordtype of
      JSON_STRING :
           begin
             line:=line +' str ='+ string(p^.svalue);
             logfct(line);
           end;
      JSON_INTEGER :
           begin
             line:=line +' int ='+ IntToStr(p^.ivalue);
             logfct(line);
           end;
      JSON_BOOLEAN :
           begin
              if (p^.bvalue) then line:=line +' bool = TRUE'
                             else line:=line +' bool = FALSE';
              logfct(line);
           end;
      JSON_STRUCT :
          begin
            logfct(line +' struct');
            for i:=0 to p^.membercount-1 do
              DumpStructureRec(p^.members^[i],deep+1,logfct);
          end;
      JSON_ARRAY :
          begin
            logfct(line +' Array');
            for i:=0 to p^.membercount-1 do
             begin
              // logfct(indent+' index '+inttostr(i)+':');
               DumpStructureRec(p^.items^[i],deep+1,logfct);
             end;
          end;
     end;
 end;



procedure TJsonParser.FreeStructure(p:PJSONRECORD);
 var i:integer;
 begin
     case p^.recordtype of
      JSON_STRING :
           begin
             freemem(p^.svalue);
             p^.svalue:=nil;
           end;
      JSON_STRUCT :
          begin
            for i:=p^.membercount-1 downto 0 do
             begin
               FreeStructure(p^.members^[i]);
               p^.members^[i] :=NIL;
             end;
             freemem(p^.members);
             p^.members         :=nil;
             p^.membercount     :=0;
             p^.memberAllocated :=0;
          end;
       JSON_ARRAY :
          begin
            for i:=p^.itemcount-1 downto 0 do
             begin
               FreeStructure(p^.items^[i]);
               p^.items^[i] :=NIL;
             end;
             freemem(p^.items);
             p^.items         :=nil;
             p^.itemcount     :=0;
             p^.itemAllocated :=0;
          end;
     end;
     freemem(p);
 end;


procedure TJsonParser.DumpStructure( logfct: Tjsonlogfct);
 begin
   DumpStructureRec(data,0,logfct);
 end;


function  TJsonParser.GetChildNode(parent:PJSONRECORD;nodename:string):PJSONRECORD;
  var
   i,index:integer;
   p:PJSONRECORD ;
  begin
   p:=parent;
   if (p=nil) then p:=data;
   if  (p^.recordtype = JSON_STRUCT)  then
    begin
     for i:=0 to  p^.membercount-1 do
      if (p^.members^[i]^.name = shortstring(nodename)) then
       begin
        GetChildNode := p^.members^[i];
        exit;
       end;
    end
   else
   if  (p^.recordtype = JSON_ARRAY)  then
    begin
     index := strtoint(nodename);
     if (index>= p^.itemcount) then  raise Exception.Create('index out of bounds '+nodename+'>='+inttostr(p^.itemcount));
      GetChildNode := p^.items^[index];
      exit;
     end;
    GetChildNode :=  NIL;
  end;

function TJsonParser.GetAllChilds(parent:PJSONRECORD):  TByteArrayArrayJson;
 var
  p : PJSONRECORD ;
  i : integer;
  res : TByteArrayArrayJson;
 begin

  p := parent;
  if (p=nil) then p:=data;
   if  (p^.recordtype = JSON_STRUCT)  then
    begin
     setlength(res,p^.membercount);
     for i:=0 to  p^.membercount-1 do
       res[i] := self.convertToBytes(p^.members^[i],false);
    end
  else
   if  (p^.recordtype = JSON_ARRAY)  then
    begin
     setlength(res,p^.itemcount);
     for i:=0 to  p^.itemcount-1 do
       res[i] := self.convertToBytes(p^.items^[i],false);
    end
  else
    setlength(res,0);

  GetAllChilds := res;
end;





function TJsonParser.Parse(st:string): PJSONRECORD;
 var
   i   : integer;
   res : PJSONRECORD;
 begin
  i   := 1;
  st:='"root" : '+St+' ';
  res := ParseEx(JWAITFORNAME,'',st,i);
  Parse :=res;
end;


end.
