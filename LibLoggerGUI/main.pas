unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus
  , LibLogger
  , GLGraf
  , KeyList
  , ArrayOfTypes
  , GLChartF
  , dglOpenGL
  , Types
  ;


type

  { TForm1 }

  TVerticalLine=record // Запись для отображения вертикальных линий маркеров
    selected:Boolean;  // признак того, что прямая выделена
    pos:Double;  // положение
    col:TValue4b; // цвет
  end;

  TForm1 = class(TForm)
    Button4: TButton;
    Button6: TButton;
    ColorDialog1: TColorDialog;
    GLSimpleChartF1: TGLSimpleChartF;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    mmiLoadTimeDiagramm: TMenuItem;
    mmiSaveTimeDiagramm: TMenuItem;
    mmiClear: TMenuItem;
    mmiReadData: TMenuItem;
    mmiFonColor: TMenuItem;
    mmiTest: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure GLSimpleChart1Exit(Sender: TObject);
    procedure GLSimpleChart1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLSimpleChart1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLSimpleChart1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSimpleChart1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSimpleChartF1AfterGLInit(Sender: TObject);
    procedure GLSimpleChartF1Paint(Sender: TObject);
    procedure mmiClearClick(Sender: TObject);
    procedure mmiFonColorClick(Sender: TObject);
    procedure mmiLoadTimeDiagrammClick(Sender: TObject);
    procedure mmiReadDataClick(Sender: TObject);
    procedure mmiSaveTimeDiagrammClick(Sender: TObject);
    procedure mmiTestClick(Sender: TObject);
  private
    FGrafs:array of TGLManyColorGraf;
    FMarkers:array of TVerticalLine; // Массив маркеров
    FMaxTime:Double; // Максимальное время в циклограмме
    FMinTime:Double; // Минимальное время в циклограмме
    FStdCol:array [0..7] of TValue4f; // 8 стандартных цветов
    FPointData:T2DArrayOfInputData;   // Массив, который будет хранить данные по каждому идентификатору
    FHintGraf:TGLSimpleGraf;          // График с подсказками, где искать отметки

    procedure ReadData;         // Процедура считывает данные из общей памяти
    procedure GetTimeMinMax;    // Разсчёт минимального и максимального времени по всем временным циклограммам
    procedure CreateGrafs;      // Создание графиков, которые отображают временные диаграммы
    procedure ShowMarkers;      // Отображение информации по маркерам
    procedure LoadDataFromFile(fn:string);  // Загрузка данных из файла

    procedure ShowTimeMinMax;
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button4Click(Sender: TObject); // Вывод данных в текстовом виде
var
  i,j:Integer;
  cnt:Cardinal;
  pid:PInputData;
  tmin:Double;
begin
  Memo1.Lines.Clear;
  for i:=0 to SMDATA_COUNT-1 do begin
    cnt:=Length(FPointData[i]);

    Memo1.Lines.Add('');
    Memo1.Lines.Add(Format('ID=%d',[i]));
    if cnt=0 then begin
      Memo1.Lines.Add('Нет данных');
      Continue;
    end;

    pid:=@FPointData[i][0];
    tmin:=pid[0].time;
    for j:=0 to cnt-1 do begin
      Memo1.Lines.Add(Format('time=%2.4f mc  col=%x',[(pid^.time-tmin)*1e3, pid^.Col]));
      Inc(pid);
    end;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject); // Шпаргалка
begin
  Memo1.Append('0 - Чёрный');
  Memo1.Append('1 - Красный');
  Memo1.Append('2 - Зелёный');
  Memo1.Append('3 - Жёлтый');
  Memo1.Append('4 - Синий');
  Memo1.Append('5 - Фиолетовый');
  Memo1.Append('6 - Бирюзовый');
  Memo1.Append('7 - Белый');

  Memo1.Append('При удержании клавиши shift, ускоряется перемещение маркеров и масштабирование');
  Memo1.Append('Клавиша "1" выделяет маркер красного цвета');
  Memo1.Append('Клавиша "2" выделяет маркер зелёного цвета');
  Memo1.Append('Клавиши право/лево (стрелки) при удержании клавиш "1" и/или "2" перемещают маркеры');
  Memo1.Append('При нажатии и удержании клавиши "3", будут отображаться подсказки, для поиска временных меток');
  Memo1.Append('При удержании клавиши Ctrl выполняется вертикальное масштабирование колесом мыши');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.PageIndex:=0;
  DoubleBuffered:=False;
  SetLength(FPointData, SMDATA_COUNT);    // Память для хранения данных по каждой циклограмме
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  LoadDataFromFile(FileNames[0]);

  GetTimeMinMax; // Разсчёт минимального и максимального значения времени во временной циклограмме
  CreateGrafs;   // Создание графиков

  GLSimpleChartF1.Invalidate;
end;

procedure TForm1.GLSimpleChart1Exit(Sender: TObject);
begin
  Keys.ClearKeys; // Удаляем все нажатые клавиши
end;

procedure TForm1.GLSimpleChart1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  dl,vx1,vx2:Double;
begin
  GLSimpleChartF1.RectToXY(Value4b(0,0), vx1, dl);
  GLSimpleChartF1.RectToXY(Value4b(1,0), vx2, dl);

  dl:=Abs(vx1-vx2); // Сдвиг по оси X, соответствующий 1 пикселю
  if ssShift in Shift then dl:=dl*5;  // Если зажимается клавиша "Shift" то сдвиг увеличивается для ускорения

  Keys.AddKey(Key); // Добавляем пришедшую клавишу в спсок для проверки

  case Key of
    49: FMarkers[0].selected:=True; // Если нажали "1", то выделяем 0-й маркер
    50: FMarkers[1].selected:=True; // Если нажали "2", то выделяем 1-й маркер
    51: ShowTimeMinMax;
  end;

  if Keys.TestKeys([49, 37], tkoIfExist) then // Зажаты клавиши "1" и стрелка вправо
    FMarkers[0].pos:=FMarkers[0].pos - dl;
  if Keys.TestKeys([49, 39], tkoIfExist) then // Зажаты клавиши "1" и стрелка вправо
    FMarkers[0].pos:=FMarkers[0].pos + dl;
  if Keys.TestKeys([50, 37], tkoIfExist) then // Зажаты клавиши "2" и стрелка вправо
    FMarkers[1].pos:=FMarkers[1].pos - dl;
  if Keys.TestKeys([50, 39], tkoIfExist) then // Зажаты клавиши "2" и стрелка вправо
    FMarkers[1].pos:=FMarkers[1].pos + dl;

  // Key:=0; - Если это раскоментировать, то даже alt+f4 не сработает, когда фокус ввода находится на этом компоненте

  GLSimpleChartF1.Invalidate;
  ShowMarkers; // Отображение вертикальных линий
end;

procedure TForm1.GLSimpleChart1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Keys.DeleteKey(Key);

  case Key of
    49: FMarkers[0].selected:=False; // Если отпустили "1", то снимаем выделеие 0-го маркера
    50: FMarkers[1].selected:=False; // Если отпустили "2", то снимаем выделеие 1-го маркера
    51: FHintGraf.Clear;             // Если отпустили "3", то удаляются посказывающие метки
  end;
  GLSimpleChartF1.Invalidate;
  ShowMarkers; // Отображение вертикальных линий
end;

procedure TForm1.GLSimpleChart1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vx,vy:Double;
begin
  if Keys.TestKeys([49]) then begin
    GLSimpleChartF1.RectToXY(Value4b(X,Y), vx, vy);
    FMarkers[0].pos:=vx;
  end;
  if Keys.TestKeys([50]) then begin
    GLSimpleChartF1.RectToXY(Value4b(X,Y), vx, vy);
    FMarkers[1].pos:=vx;
  end;
  GLSimpleChartF1.Invalidate;
end;

procedure TForm1.GLSimpleChart1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  m:Single;
begin
  if ssShift in Shift then m:=0.5 else m:=0.1;

  // Масштабирование колесом только по одной координате
  if ssCtrl in Shift then begin
      if WheelDelta<0 then
        GLSimpleChartF1.WheelZoom(MousePos, 1, 1-m)  // Масштабирование координатной системы
      else
        GLSimpleChartF1.WheelZoom(MousePos, 1, 1+m);  // Масштабирование координатной системы
  end else begin
    if WheelDelta<0 then
      GLSimpleChartF1.WheelZoom(MousePos, 1-m, 1)  // Масштабирование координатной системы
    else
      GLSimpleChartF1.WheelZoom(MousePos, 1+m, 1);  // Масштабирование координатной системы
  end;

  Handled:=True; // Признак того, что событие обработанно полностью, поэтому дальнейшая обработка в родительском классе не будет выполняться
  GLSimpleChartF1.Invalidate;
end;

procedure TForm1.GLSimpleChartF1AfterGLInit(Sender: TObject);
var
  i:Integer;
begin
  GLSimpleChartF1.ClearColor:=GLSimpleChartF1.RGBAToColor(178, 178, 178, 255);

  SetLength(FMarkers, 2); // Выделение памяти для 2-х маркеров
  FMarkers[0].col:=Value4b(255,0,0,255);
  FMarkers[0].pos:=-0.1;
  FMarkers[0].selected:=False;

  FMarkers[1].col:=Value4b(0,255,0,255);
  FMarkers[1].pos:=0.1;
  FMarkers[1].selected:=False;

  // Выделение памяти для объектов-графиков
  SetLength(FGrafs, SMDATA_COUNT);
  for i:=0 to SMDATA_COUNT-1 do begin
    FGrafs[i]:=TGLManyColorGraf.Create;
    FGrafs[i].DrawMode:=GL_QUADS;
  end;

  FHintGraf:=TGLSimpleGraf.Create(0,0,0);
  FHintGraf.DrawMode:=GL_LINES;

  GLSimpleChartF1.Font.SetFont;

  // Формируем 8 стандартных цветов
  FStdCol[0]:=Value4f(0,0,0,1);
  FStdCol[1]:=Value4f(1,0,0,1);
  FStdCol[2]:=Value4f(0,1,0,1);
  FStdCol[3]:=Value4f(1,1,0,1);
  FStdCol[4]:=Value4f(0,0,1,1);
  FStdCol[5]:=Value4f(1,0,1,1);
  FStdCol[6]:=Value4f(0,1,1,1);
  FStdCol[7]:=Value4f(1,1,1,1);
end;

procedure TForm1.GLSimpleChartF1Paint(Sender: TObject);
var
  i,j:Integer;
begin
  GLSimpleChartF1.ClearScreen;

    // Отрисовка циклограммы
  for i:=0 to SMDATA_COUNT-1 do // Цикл для всех массивов точек циклограмм
    GLSimpleChartF1.DrawGrafVBO(FGrafs[i]);

  for i:=0 to Length(FMarkers)-1 do begin // Цикл по вертикальным линиям на шкале времени
    with FMarkers[i] do begin
      if selected then
        glLineWidth(6)
      else
        glLineWidth(1);

      GLSimpleChartF1.BeginDrawUser(GL_LINES, Value3f(col.a/255, col.b/255, col.c/255));
      glVertex2f(pos, 0);
      glVertex2f(pos, 10);
      GLSimpleChartF1.EndDrawUser;

      glColor3ub(0,0,0);
      GLSimpleChartF1.TextOutSS(pos, 0.05, 0.08, 0.08, Format('0[%d]',[i+1]));
      GLSimpleChartF1.TextOutSS(pos, 10, 0.08, 0.08, Format('[%d]',[i+1]));

      for j:=1 to 9 do
        GLSimpleChartF1.TextOutSS(pos, j+0.05, 0.08, 0.08, Format('%d',[j]));
    end;
  end;

  GLSimpleChartF1.DrawGrafVBO(FHintGraf);

  GLSimpleChartF1.DrawAxis;
  GLSimpleChartF1.DrawScaleFrame;
  GLSimpleChartF1.SwapBuffers;
end;

procedure TForm1.mmiClearClick(Sender: TObject);
var
  i:Integer;
begin
  ResetLogger;
  for i:=0 to Length(FGrafs)-1 do // Удаляем графики
    FGrafs[i].Clear;
  GLSimpleChartF1.Invalidate;
end;

procedure TForm1.mmiFonColorClick(Sender: TObject);
var
  r,g,b:Byte;
begin
  if not ColorDialog1.Execute then Exit;

  RedGreenBlue(ColorDialog1.Color, r,g,b);
  GLSimpleChartF1.ClearColor:=GLSimpleChartF1.RGBAToColor(r, g, b, 255);
  GLSimpleChartF1.Invalidate;
end;

procedure TForm1.mmiLoadTimeDiagrammClick(Sender: TObject); // Загрузка временной диаграммы
begin
  if not OpenDialog1.Execute then Exit;
  LoadDataFromFile(OpenDialog1.FileName);

  GetTimeMinMax; // Разсчёт минимального и максимального значения времени во временной циклограмме
  CreateGrafs;   // Создание графиков

  GLSimpleChartF1.Invalidate;
end;

procedure TForm1.mmiReadDataClick(Sender: TObject);
begin
  ReadData;       // Тут выполняется загрузка из общей памяти записей циклограммы
  GetTimeMinMax;  // Поиск минимального и максимального значения времени
  CreateGrafs;    // Создание графиков
  GLSimpleChartF1.Invalidate;  // Перерисовка
end;

procedure TForm1.mmiSaveTimeDiagrammClick(Sender: TObject); // Сохранение временной диаграммы
var
  i,cnt:Integer;
  fs:TFileStream;
  str:string;
begin
  if not SaveDialog1.Execute then Exit;

  fs:=TFileStream.Create(SaveDialog1.FileName, fmCreate);

  str:='Time diagram by logger'+#13; fs.WriteBuffer(str[1], Length(str));
  str:='Data offset=1000 byte'+#13;  fs.WriteBuffer(str[1], Length(str));
  str:='Format:'+#13;                fs.WriteBuffer(str[1], Length(str));
  str:='Point count - uint32_t'+#13; fs.WriteBuffer(str[1], Length(str));
  str:='Point count * [Col:uint32_t; tag:uint32_t; time:Double]'; fs.WriteBuffer(str[1], Length(str));

  fs.Position:=1000;

  for i:=0 to SMDATA_COUNT-1 do begin
    cnt:=Length(FPointData[i]);
    fs.WriteDWord(cnt);
    if cnt=0 then Continue;
    fs.WriteBuffer(FPointData[i][0], cnt*SizeOf(TInputData));
  end;

  fs.Free;
end;

procedure TForm1.mmiTestClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to 10 do
    begin
      SendDigitLog(0, i mod 4, 0);
      Sleep(5);
    end;
  for i:=0 to 10 do
    begin
      SendDigitLog(1, i mod 8, 0);
      Sleep(10);
    end;
  for i:=0 to 15 do
    begin
      SendDigitLog(3, i mod 4, 0);
      Sleep(20);
    end;
  for i:=0 to 2 do
    begin
      SendDigitLog(6, RGBToColor(0,   0,      0   ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   0,      127 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   0,      255 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   127,    0   ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   127,    255 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   255,    0   ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   255,    127 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(0,   255,    255 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(127, 0,      127 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(127, 0,      255 ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(127, 127,    0   ), 0);
      Sleep(10);
      SendDigitLog(6, RGBToColor(127, 127,    255 ), 0);
      Sleep(10);
    end;

  for i:=0 to 2 do
    begin
      SendDigitLog(7, RGBToColor(0,   0,      0   ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(0,   0,      255 ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(0,   255,    0   ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(0,   255,    255 ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(255, 0,      0   ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(255, 0,      255 ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(255, 255,    0   ), 0);
      Sleep(10);
      SendDigitLog(7, RGBToColor(255, 255,    255 ), 0);
      Sleep(10);
    end;
end;

procedure TForm1.ReadData; // Процедура считывает данные из общей памяти
var
  str:string;
  i:Integer;
  cnt:Cardinal;
  pid1:PInputData;
begin
  // Тут выполняется загрузка из общей памяти записей циклограммы

  // Далее выделяем память под точки циклограмм и считываем данные из общей памяти
  str:='';
  for i:=0 to SMDATA_COUNT-1 do begin
    cnt:=PCardinal(AddrSMControl())[i]; // Кол-во точек в i-й циклограмме
    FPointData[i]:=nil;
    if cnt=0 then Continue;
    str:=str+Format('id=%d (%d)__',[i, cnt]);
    SetLength(FPointData[i], cnt);

    pid1:=PInputData(AddrSMData(i));  // Получаем адрес на данные для i-й циклограммы
    Move(pid1^, FPointData[i][0], cnt*SizeOf(TInputData)); // Копирование данных из общей памяти
  end;

  if str='' then
    Label1.Caption:='Нет данных'
  else
    Memo1.Append(str);
end;

procedure TForm1.GetTimeMinMax; // Разсчёт минимального и максимального времени по всем временным циклограммам
var
  i,cnt:Integer;
  tval1:Double;
begin
  // Расчёт максимального и минимального времени (минимум и максимум определяется только по 1-й и последней записи, т.к. время всегда только увеличивается)
  FMaxTime:=-1e20;
  FMinTime:=1e20;
  for i:=0 to SMDATA_COUNT-1 do begin // Цикл по циклограммам
    cnt:=Length(FPointData[i]);
    if cnt=0 then Continue;
    tval1:=FPointData[i,0].time;
    if FMinTime>tval1 then FMinTime:=tval1;
    tval1:=FPointData[i,cnt-1].time;
    if FMaxTime<tval1 then FMaxTime:=tval1;
  end;
end;

procedure TForm1.CreateGrafs; // Создание графиков, которые отображают временные диаграммы
var
  i,j:Integer;
  cnt:Cardinal;
  pid1,pid2:PInputData;
  uplvl,downlvl:Single;
  tval1,tval2:Double;
  grf:TGLManyColorGraf;
  col:TValue4b;
  col4f:TValue4f;
begin
  // После того, как данные скопированны из общей памяти, (начинается самое сложное) необходимо сформировать точки для отображения циклограммы
  for i:=0 to SMDATA_COUNT-1 do begin // Цикл по циклограммам
    cnt:=Length(FPointData[i]);
    if cnt=0 then Continue;

    grf:=FGrafs[i]; // Далее будем изпользовать переменную grf вместо FGrf[i]
    grf.GetMem4NPoints(cnt*3); // Выделяем память на количество точек с запасом
    grf.Clear;     // Удаление всех точек в графике для i-й циклограммы

    pid1:=@FPointData[i][0]; // Это будет текущая точка
    pid2:=@FPointData[i][1]; // Это будет следующая точка

    for j:=0 to cnt-1 do begin // Цикл по всем считанным точкам циклограммы
      // pid1^ - Текущая запись TinputData
      // pid2^ - Следующая запись TinputData

      tval1:=pid1^.time-FMinTime; // Время на шкале времени текущих точек прямоугольника
      if j<>cnt-1 then
        tval2:=pid2^.time-FMinTime // Время на шкале времени следующих точек прямоугольника
      else
        tval2:=tval2+0.001; // для последнего прямоугольника время его левых точк определяется через последнее увеличенное на 1 мс

      if pid1^.Col<8 then begin // Если номер цвета меньше 8, то считается, что это индекс стандартного цвета
        col4f:=FStdCol[pid1^.Col];
        downlvl:=i+0.3+pid1^.Col*0.0875;
        uplvl:=downlvl+0.0874;

        grf.AddPoint(tval1, uplvl, 1, col4f);
        grf.AddPoint(tval1, downlvl, 1, col4f);

        grf.AddPoint(tval2, downlvl, 1, col4f);
        grf.AddPoint(tval2, uplvl, 1, col4f);
      end else begin // иначе понимаем номер цвета, как сам цвет
        col.UInt:=pid1^.Col or $ff000000;

        grf.AddPoint(tval1, i+0.3, 1, col);
        grf.AddPoint(tval1, i, 1, col);

        grf.AddPoint(tval2, i, 1, col);
        grf.AddPoint(tval2, i+0.3, 1, col);
      end;

      Inc(pid1); Inc(pid2); // Сдвигаем указатель для текущей и следующей записи
    end;

    grf.UpdateVBOData();
  end;

  if FMaxTime>FMinTime then
    GLSimpleChartF1.ZoomRectCS(0, 9, FMaxTime-FMinTime, 0, True);
end;

procedure TForm1.ShowMarkers;
var
  dl:Double;
begin
  dl:=FMarkers[0].pos-FMarkers[1].pos;
  if Abs(dl)>1 then begin
    Label1.Caption:=Format('Pos1=%2.5f   Pos2=%2.5f   dP=%2.3f сек',[FMarkers[0].pos, FMarkers[1].pos, dl]);
    Exit;
  end;
  if Abs(dl)>1e-3 then begin
    Label1.Caption:=Format('Pos1=%2.5f   Pos2=%2.5f   dP=%2.3f мс',[FMarkers[0].pos, FMarkers[1].pos, dl*1e3]);
    Exit;
  end;
  if Abs(dl)>1e-6 then begin
    Label1.Caption:=Format('Pos1=%2.5f   Pos2=%2.5f   dP=%2.3f мкс',[FMarkers[0].pos, FMarkers[1].pos, dl*1e6]);
    Exit;
  end;
end;

procedure TForm1.LoadDataFromFile(fn: string); // Загрузка данных из файла
var
  i,cnt:Integer;
  fs:TFileStream;
begin
  fs:=TFileStream.Create(fn, fmOpenRead);
  if fs.Size<1000 then begin // Если размер файла меньше смещения с которого начинаются данные, то выходим (файл не тот)
    fs.Free;
    Exit;
  end;
  fs.Position:=1000;

  for i:=0 to SMDATA_COUNT-1 do begin
    cnt:=fs.ReadDWord; // Сначала лежит кол-во записей для временной циклограммы
    SetLength(FPointData[i], cnt);
    if cnt=0 then Continue;
    fs.ReadBuffer(FPointData[i][0], cnt*SizeOf(TInputData));
  end;

  fs.Free;
end;

procedure TForm1.ShowTimeMinMax;
var
  vx1,vx2,vy:Double;
  tm:Double;
  i,j:Integer;
begin
  GLSimpleChartF1.RectToXY(Value4b(0,0), vx1, vy);
  GLSimpleChartF1.RectToXY(Value4b(GLSimpleChartF1.Width,0), vx2, vy);

  FHintGraf.Clear;
  for i:=0 to SMDATA_COUNT-1 do begin             // Идентификатор диаграммы
    for j:=0 to Length(FPointData[i])-1 do begin
      tm:=FPointData[i,j].time - FMinTime;
      if (vx1<tm)and(tm<vx2) then begin
        FHintGraf.AddPoint(tm, i);
        FHintGraf.AddPoint(tm, i+1);
      end;
    end;
  end;
  FHintGraf.UpdateVBOData;
end;

end.

