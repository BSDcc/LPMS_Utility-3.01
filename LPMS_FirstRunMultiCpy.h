//---------------------------------------------------------------------------

#ifndef LPMS_FirstRunMultiCpyH
#define LPMS_FirstRunMultiCpyH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvCipher.hpp"
#include "JvComponentBase.hpp"
//---------------------------------------------------------------------------
class TFLPMS_FirstRunMultiCpy : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TEdit *edtDBPrefix;
   TCheckBox *cbMultiCpy;
   TButton *btnOK;
   TJvVigenereCipher *jvCipher;
   TButton *btnCancel;
   void __fastcall FormActivate(TObject *Sender);
   void __fastcall edtDBPrefixChange(TObject *Sender);
   void __fastcall btnOKClick(TObject *Sender);
   void __fastcall btnCancelClick(TObject *Sender);
private:	// User declarations
   bool       MultiCompany;
   AnsiString DBPrefix;

public:		// User declarations
   __fastcall TFLPMS_FirstRunMultiCpy(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFLPMS_FirstRunMultiCpy *FLPMS_FirstRunMultiCpy;
//---------------------------------------------------------------------------
#endif
