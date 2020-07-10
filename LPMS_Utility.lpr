program LPMS_Utility;

{$mode objfpc}{$H+}

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, datetimectrls, LPMS_UtilityApp, LPMS_InputQuery, LPMS_UtilityMultiCpy,
   indylaz, Classes, LPMS_UtilitySelDB
   { you can add units after this };

{$R *.res}

begin
   RequireDerivedFormResource:=True;
   Application.Scaled:=True;
   Application.Initialize;
   Application.CreateForm(TFLPMS_UtilityApp, FLPMS_UtilityApp);
   Application.CreateForm(TFLPMS_UtilityMultiCpy, FLPMS_UtilityMultiCpy);
   Application.CreateForm(TFLPMS_UtilitySelDB, FLPMS_UtilitySelDB);

//--- Allocate lists that must be deleted when the program ends

   try

      ImportList := TStringList.Create;
      TableList  := TStringList.Create;

      Application.Run;

   finally

      ImportList.Free;
      TableList.Free;

   end;

end.

