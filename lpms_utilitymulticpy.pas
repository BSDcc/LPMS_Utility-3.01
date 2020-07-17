//------------------------------------------------------------------------------
// Date.......: 02 July 2020
// System.....: Legal Practice Management System - General Utility
// Program ID.: LPMS_UtilityMulitCpy
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// Description: This form is used to select the company to work with
//------------------------------------------------------------------------------
// History....:02 July 2020 - Adapt from LPMS C++ version of LPMS_FirstRun
//------------------------------------------------------------------------------

unit LPMS_UtilityMultiCpy;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
   ExtCtrls, Buttons,

{$IFDEF WINDOWS}                     // Target is Winblows
   Registry;
{$ELSE}                              // Target is *nix or macOS
   IniFiles;
{$ENDIF}

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_UtilityMultiCpy }

   TFLPMS_UtilityMultiCpy = class(TForm)
   Bevel1: TBevel;
      btnCancel: TButton;
      btnLockS: TSpeedButton;
      btnOK: TButton;
      btnUnlockS: TSpeedButton;
      cbMultiCpy: TCheckBox;
      cbUpdateEncoding: TCheckBox;
      edtDBPrefix: TEdit;
      Label1: TLabel;
      procedure btnCancelClick(Sender: TObject);
      procedure btnOKClick(Sender: TObject);
      procedure edtDBPrefixChange(Sender: TObject);
      procedure FormShow(Sender: TObject);

private  { Private Declarations }
   MultiCompany : boolean;          // Indicates whether Multi Company was selected;
   DBPrefix     : string;           // Holds the chosen DBPrefix

public   { Public Declartions}

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
  CYPHER_ENC = 0;
  CYPHER_DEC = 1;

//--- Temporary - Allow both old style encoding and new style encoding but
//--- only one type at a time

//{$DEFINE OLD_ENCODING}

var
   FLPMS_UtilityMultiCpy: TFLPMS_UtilityMultiCpy;

{$IFDEF DARWIN}
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; external 'libbsd_utilities.dylib';
{$ENDIF}
{$IFDEF WINDOWS}
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; external 'BSD_Utilities.dll';
{$ENDIF}
{$IFDEF LINUX}
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; external 'libbsd_utilities.so';
{$ENDIF}

implementation

   uses LPMS_UtilityApp;

{$R *.lfm}

   { TFLPMS_UtilityMultiCpy }

{==============================================================================}
{--- General functions                                                      ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Executed when the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityMultiCpy.FormShow(Sender: TObject);
var
   ThisDBPrefix : string;
{$IFDEF WINDOWS}
   RegString    : string = 'Software\BlueCrane Software\LPMS 3';
   RegIni       : TRegistryIniFile;
{$ELSE}
   RegString    : string = 'LPMS 3.ini';
   RegIni       : TINIFile;
{$ENDIF}

begin

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create(RegString);
{$ELSE}
   RegIni := TINIFile.Create(FLPMS_UtilityApp.RegPath + RegString);
{$ENDIF}

   MultiCompany := RegIni.ReadBool('Preferences','MultiCompany',False);
   DBPrefix     := RegIni.ReadString('Preferences','DBPrefix','invalid');

   RegIni.Destroy;

{$IFDEF OLD_ENCODING}
   ThisDBPrefix := jvCipher.DecodeString(FLPMS_UtilityApp.ThisPass,DBPrefix);
{$ELSE}
   ThisDBPrefix := Vignere(CYPHER_DEC,Copy(DBPrefix,1,6),FLPMS_UtilityApp.SecretPhrase);
{$ENDIF}

  if MultiCompany = True then begin

     cbMultiCpy.Checked := True;
     edtDBPrefix.Text   := '';
     btnOK.Enabled      := False;

  end else begin

     cbMultiCpy.Checked := False;
     edtDBPrefix.Text   := Copy(ThisDBPrefix,1,6);

  end;

  edtDBPrefix.SetFocus();

end;

//------------------------------------------------------------------------------
// User changed the DBPrefix
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityMultiCpy.edtDBPrefixChange(Sender: TObject);
begin

   if Length(edtDBPrefix.Text) = 6 then
      btnOK.Enabled := True
   else
      btnOK.Enabled := False;

end;

//------------------------------------------------------------------------------
// User clicked on the 'OK' button
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityMultiCpy.btnOKClick(Sender: TObject);
var
   KeyExists               : boolean;
   ThisDBPrefix, RegString : string;
{$IFDEF WINDOWS}
   RegIni       : TRegistryIniFile;
{$ELSE}
   RegIni       : TINIFile;
{$ENDIF}

begin

//--- Check whether the company that was selected exists

   if cbMultiCpy.Checked = True then begin

{$IFDEF WINDOWS}
      RegString := 'Software\BlueCrane Software\LPMS 3\' + edtDBPrefix.Text;
      RegIni        := TRegistryIniFile.Create(RegString);
{$ELSE}
      RegString := FLPMS_UtilityApp.RegPath + edtDBPrefix.Text + '.ini';
      RegIni        := TINIFile.Create(RegString);
{$ENDIF}

   end else begin

{$IFDEF WINDOWS}
      RegString := 'Software\BlueCrane Software\LPMS 3';
      RegIni    := TRegistryIniFile.Create(RegString);
{$ELSE}
      RegString := FLPMS_UtilityApp.RegPath + 'LPMS 3.ini';
      RegIni    := TINIFile.Create(RegString);
{$ENDIF}

   end;

   KeyExists := RegIni.ValueExists('Preferences','DBPrefix');

   if KeyExists = True then begin

      ThisDBPrefix := RegIni.ReadString('Preferences','DBPrefix','******');

{$IFDEF OLD_ENCODING}
      ThisDBPrefix := jvCipher.DecodeString(FLPMS_UtilityApp.ThisPass,ThisDBPrefix);
{$ELSE}
      ThisDBPrefix := Vignere(CYPHER_DEC,Copy(ThisDBPrefix,1,6),FLPMS_UtilityApp.SecretPhrase);
{$ENDIF}

      if (edtDBPrefix.Text = Copy(ThisDBPrefix,1,6)) or (Copy(ThisDBPrefix,1,6) = 'invali') then begin

         FLPMS_UtilityApp.ThisDBPrefix  := edtDBPrefix.Text;
         FLPMS_UtilityApp.KeepRegString := RegString;

         RegIni.Destroy;
         Close;
         Exit;

      end;

   end;

   Application.MessageBox('''DBPrefix'' is invalid or spelled incorrectly.','LPMS Utility',(MB_OK + MB_ICONSTOP));
   RegIni.Destroy;
   Exit;

end;

//------------------------------------------------------------------------------
// User clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityMultiCpy.btnCancelClick(Sender: TObject);
begin

   FLPMS_UtilityApp.Proceed := False;
   Close;

end;

//------------------------------------------------------------------------------
end.

