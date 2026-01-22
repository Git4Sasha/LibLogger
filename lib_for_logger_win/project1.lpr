library project1;

{$mode objfpc}{$H+}

uses
  LocalTimeWork, SysUtils, ShareMemForProcess;

const
  SMDATA_COUNT=10; // Количество идентификаторов, для которых будет отслеживание
  SMDATA_RECCOUNT=50000; // Количество записей типа TInputData в общей памяти SMData

type
  TInputData=record
    Col:Integer;
    tag:Integer;
    time:Double;
  end;
  PInputData=^TInputData;

var
  LastError:Integer; // Номер ошибки последней операции
  SMControl:TShareMemProcess; // Общая память управляющих данных (здесь будет храниться количество данных для каждого идентификатора)
  SMData:array [0..SMDATA_COUNT-1] of TShareMemProcess; // На каждый идентификатор свой массив
  CPUTp:Double;  // Период работы процессора

procedure SendDigitLog(id:Integer; style:Integer; tag: Integer=0); stdcall;
var
  buf:PInputData;
  pcnt:PCardinal;
begin
  pcnt:=SMControl.Addr;
  Inc(pcnt, id);  // Получаем указатель на ячейку в которой лежит количество записей для заданного идентификатора
  if pcnt^>=SMDATA_RECCOUNT then // Если количество записей достигло предела, то
    Exit; // выходим и не добавляем новых записей
  buf:=SMData[id].Addr;
  Inc(buf, pcnt^); // Получаем указатель на то место в общей памяти, в которое можно сохранить, новую запись
  // Заполняем ячейку данными
  buf^.Col:=style;
  buf^.tag:=tag;
  buf^.time:=GetTickCountCPU*CPUTp;

  Inc(pcnt^); // Увеличиваем количество записей
end;

procedure ResetLogger; // Сброс всех точек в общей памяти
var
  i:Integer;
  pcnt:PCardinal;
begin
  pcnt:=SMControl.Addr;
  for i:=0 to SMDATA_COUNT-1 do
    begin
      FillQWord(SMData[i].Addr^, pcnt[i], 0);
      pcnt[i]:=0;
    end;
end;

function AddrSMControl:Pointer;
begin
  Result:=SMControl.Addr;
end;

function AddrSMData(id:Integer):Pointer; stdcall;
begin
  Result:=SMData[id].Addr;
end;

function GetLastError:Integer;
begin
  Result:=LastError;
end;

procedure InitLogger;
var
  i:Integer;
begin
  SMControl:=TShareMemProcess.Create('SMControlLogger',SizeOf(Cardinal)*SMDATA_COUNT); // Общая память управляющих данных (здесь будет храниться количество данных для каждого идентификатора)
  LastError:=SMControl.Error;
  if LastError<>0 then // Если есть ошибка, то
    begin  // удаляем созданный объект и выходим
      SMControl.Free;
      Exit;
    end;

  for i:=0 to SMDATA_COUNT-1 do
    begin
      SMData[i]:=TShareMemProcess.Create(Format('SMDataLogger%d',[i]), SizeOf(TInputData)*SMDATA_RECCOUNT);
      LastError:=SMData[i].Error;
      if LastError<>0 then // Если есть ошибка, то
        Exit;  // выходим
    end;

  CPUTp:=1.0/GetCPUFreq; // Получаем частоту работы процессора (эта процедура замораживает работу программы на 500 мс)
end;

procedure DeinitLogger;
var
  i:Integer;
begin
  // Удаляем объекты общей памяти (если эти объекты ещё изпользуются другими процессами, то они не будут удалены)
  SMControl.Free;
  for i:=0 to SMDATA_COUNT-1 do
    SMData[i].Free;
end;

exports
  GetLastError,
  InitLogger,
  DeinitLogger,
  SendDigitLog,
  ResetLogger,
  AddrSMControl,
  AddrSMData;


begin
end.

