//---------------------------------------------------------------------------

#ifndef LPMS_FirstRunldSelDBH
#define LPMS_FirstRunldSelDBH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TFLPMS_FirstRunSelDB : public TForm
{
__published:	// IDE-managed Components
   TButton *btnReturn;
   TButton *btnConnect;
   TLabel *Label12;
   TEdit *edtHostname;
   TEdit *edtUsername;
   TLabel *Label1;
   TEdit *edtPassword;
   TLabel *Label2;
   TStaticText *edtStatic;
   TImageList *imgSmall;
   void __fastcall edtHostnameChange(TObject *Sender);
   void __fastcall btnConnectClick(TObject *Sender);
   void __fastcall btnReturnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TFLPMS_FirstRunSelDB(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFLPMS_FirstRunSelDB *FLPMS_FirstRunSelDB;
//---------------------------------------------------------------------------
#endif
