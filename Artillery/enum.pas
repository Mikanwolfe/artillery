program enums;
uses
  SysUtils, typinfo;
  
type
  TMyEnum = (One, Two, Three);
var
  MyEnum : TMyEnum;
  EnumVal:Integer;
  Message:String;
begin
  MyEnum := Two;
  EnumVal:= Ord(MyEnum);
  writeln(EnumVal);  // writes 1, because first element in enumeration is numbered zero

  MyEnum := TMyEnum(EnumVal);  // Use TMyEnum as if it were a function
  str(TMyEnum(EnumVal),message);
  Writeln (message);  //  Use RTTI to return the enum value's name
  readln;
end.