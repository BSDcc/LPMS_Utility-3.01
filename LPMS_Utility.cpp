//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("LPMS_Decode.cpp", DldDecode); /* TDataModule: File Type */
USEFORM("LPMS_FirstRunldSelDB.cpp", FLPMS_FirstRunSelDB);
USEFORM("LPMS_InputQueryM.cpp", FldInputQueryM);
USEFORM("LPMS_FirstRunMultiCpy.cpp", FLPMS_FirstRunMultiCpy);
USEFORM("LPMS_FirstRunApp.cpp", FLPMS_FirstRunApp);
//---------------------------------------------------------------------------
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
   try
   {
       Application->Initialize();
       Application->MainFormOnTaskBar = true;
       Application->CreateForm(__classid(TFLPMS_FirstRunApp), &FLPMS_FirstRunApp);
       Application->CreateForm(__classid(TDldDecode), &DldDecode);
       Application->CreateForm(__classid(TFLPMS_FirstRunMultiCpy), &FLPMS_FirstRunMultiCpy);
       Application->CreateForm(__classid(TFLPMS_FirstRunSelDB), &FLPMS_FirstRunSelDB);
       Application->CreateForm(__classid(TFldInputQueryM), &FldInputQueryM);
       Application->Run();
   }
   catch (Exception &exception)
   {
       Application->ShowException(&exception);
   }
   catch (...)
   {
       try
       {
          throw Exception("");
       }
       catch (Exception &exception)
       {
          Application->ShowException(&exception);
       }
   }
   return 0;
}
//---------------------------------------------------------------------------
