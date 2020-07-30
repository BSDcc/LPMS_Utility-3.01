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
   edtPort: TEdit;
   edtUsername: TEdit;
   imgSmall: TImageList;
   Label1: TLabel;
   Label12: TLabel;
   Label2: TLabel;
   Label3: TLabel;
   Label4: TLabel;
   Label5: TLabel;
   procedure btnConnectClick(Sender: TObject);
   procedure btnReturnClick(Sender: TObject);
   procedure edtHostnameChange(Sender: TObject);

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

   uses LPMS_UtilityApp;

{$R *.lfm}

{ TFLPMS_UtilitySelDB }

//------------------------------------------------------------------------------
// Function to make sure the user provides all required information
//------------------------------------------------------------------------------
procedure TFLPMS_UtilitySelDB.edtHostnameChange(Sender: TObject);
begin

  if ((Trim(edtUsername.Text) = '') or (Trim(edtPassword.Text) = '') or (Trim(edtHostname.Text) = '')) then begin

     btnConnect.Enabled := False;
     btnConnect.Default := False;

  end else begin

     btnConnect.Enabled := True;
     btnConnect.Default := True;

  end;

end;

//------------------------------------------------------------------------------
// User clicked on the Restore button
//------------------------------------------------------------------------------
procedure TFLPMS_UtilitySelDB.btnConnectClick(Sender: TObject);
begin

   FLPMS_UtilityApp.RestoreHost := edtHostname.Text;
   FLPMS_UtilityApp.RestoreUser := edtUsername.Text;
   FLPMS_UtilityApp.RestorePass := edtPassword.Text;

   if Trim(edtPort.Text) = '' then
      FLPMS_UtilityApp.RestorePort := '3306'
   else
      FLPMS_UtilityApp.RestorePort := edtPort.Text;

   FLPMS_UtilityApp.DoRest   := True;

   Close;

end;

//---------------------------------------------------------------------------
// User clicked on the Cancel button
//---------------------------------------------------------------------------
procedure TFLPMS_UtilitySelDB.btnReturnClick(Sender: TObject);
begin

  FLPMS_UtilityApp.DoRest := False;
  Close;

end;

//------------------------------------------------------------------------------
end.
