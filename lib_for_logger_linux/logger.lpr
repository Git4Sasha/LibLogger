library logger;

{$mode objfpc}{$H+}

uses
  LocalTimeWork, SysUtils, ShareMemForProcess;

const
  SMDATA_COUNT=10; // Количество идентификаторов, для которых будет отслеживание
  SMDATA_RECCOUNT=50000; // Количество записей типа TInputData в общей памяти SMData
  SMControlLogger=8767; // По этому ключу будет выполняться доступ к общей памяти с данными о метках (S-8; M-7; C-6; L-7)

type
  TInputData=record
    Col:Integer;
    tag:Integer;
    time:Double;
  end;
  PInputData=^TInputData;

var
  InintLog:Boolean =False;  // Признак того, что логгер был проинициализирован
  SMControl:TShareMemProcess; // Общая память управляющих данных (здесь будет храниться количество данных для каждого идентификатора)
  SMData:array [0..SMDATA_COUNT-1] of TShareMemProcess; // На каждый идентификатор свой массив

procedure SendDigitLog(id:Integer; style:Integer; tag: Integer=0); cdecl;
var
  buf:PInputData;
  pcnt:PCardinal;
begin
  if not InintLog then Exit; // Если не было инициализации логгера, то выходим

  pcnt:=SMControl.Addr;
  Inc(pcnt, id);  // Получаем указатель на ячейку в которой лежит количество записей для заданного идентификатора
  if pcnt^>=SMDATA_RECCOUNT then // Если количество записей достигло предела, то
    Exit; // выходим и не добавляем новых записей
  buf:=SMData[id].Addr;
  Inc(buf, pcnt^); // Получаем указатель на то место в общей памяти, в которое можно сохранить, новую запись
  // Заполняем ячейку данными
  buf^.Col:=style;
  buf^.tag:=tag;
  buf^.time:=GetClockTimeValue;

  Inc(pcnt^); // Увеличиваем количество записей
end;

procedure ResetLogger; // Сброс всех точек в общей памяти
var
  i:Integer;
  pcnt:PCardinal;
begin
  if not InintLog then Exit;

  pcnt:=SMControl.Addr;
  for i:=0 to SMDATA_COUNT-1 do
    begin
      FillQWord(SMData[i].Addr^, pcnt[i], 0);
      pcnt[i]:=0;
    end;
end;

function AddrSMControl:Pointer;
begin
  if InintLog then
    Result:=SMControl.Addr
  else
    Result:=nil;
end;

function AddrSMData(id:Integer):Pointer; cdecl;
begin
  if InintLog then
    Result:=SMData[id].Addr
  else
    Result:=nil;
end;

function InitLogger(log:Boolean):Integer; // log - Признак того, что нужно отображать текстовую информацию при инициализации
var
  i:Integer;
begin
  Result:=0;
  if InintLog then Exit; // если инициализация уже выполнялась, то ничего не делаем

  WriteLn(Format('LibLogger ShareMem ID = %x', [SMControlLogger]));

  SMControl:=TShareMemProcess.Create(SMControlLogger, SizeOf(Cardinal)*SMDATA_COUNT); // Общая память управляющих данных (здесь будет храниться количество данных для каждого идентификатора)
  if log then WriteLn('SMControl.Error= ',SMControl.Error);
  if SMControl.Error<>0 then Exit(-1);

  for i:=0 to SMDATA_COUNT-1 do begin
    SMData[i]:=TShareMemProcess.Create(SMControlLogger+1+i, SizeOf(TInputData)*SMDATA_RECCOUNT);
    if log then WriteLn('SMData[',i,'].Error= ', SMData[i].Error);
    if SMData[i].Error<>0 then Exit(-2-i);
  end;
  InintLog:=True;
end;

procedure DeinitLogger;
var
  i:Integer;
begin
  if not InintLog then Exit;

  // Удаляем объекты общей памяти (если эти объекты ещё изпользуются другими процессами, то они не будут удалены)
  SMControl.Free;
  for i:=0 to SMDATA_COUNT-1 do
    SMData[i].Free;

  InintLog:=False;
end;

exports
  InitLogger,
  DeinitLogger,
  SendDigitLog,
  ResetLogger,
  AddrSMControl,
  AddrSMData;


begin
end.

