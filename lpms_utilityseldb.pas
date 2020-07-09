//------------------------------------------------------------------------------
// Date.......: 09 July 2020
// System.....: Legal Practice Management System - General Utility
// Program ID.: LPMS_UtilitySelDB
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// Description: This form is used to select the host to which the backup must
// ...........: be restored
//------------------------------------------------------------------------------
// History....:09 July 2020 - Adapt from LPMS C++ version of LPMS_FirstRun
//------------------------------------------------------------------------------

unit LPMS_UtilitySelDB;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
   Buttons;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_UtilitySelDB }

   TFLPMS_UtilitySelDB = class(TForm)
   Bevel1: TBevel;
   btnConnect: TBitBtn;
   btnReturn: TBitBtn;
   edtHostname: TEdit;
   edtPassword: TEdit;
   edtStatic: TStaticText;
   edtUsername: TEdit;
   imgSmall: TImageList;
   Label1: TLabel;
   Label12: TLabel;
   Label2: TLabel;

private { Private Declarations }

public  { Public Declarations }

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
//const
//
var
   FLPMS_UtilitySelDB: TFLPMS_UtilitySelDB;

implementation

{$R *.lfm}

//------------------------------------------------------------------------------
// Executed when the Form is shown
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
end.

