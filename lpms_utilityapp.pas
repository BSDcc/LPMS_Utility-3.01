//------------------------------------------------------------------------------
// Date.......: 28 June 2020
// System.....: Legal Practice Management System - General Utility
// Program ID.: LPMS_UtilityApp
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// Description: This is the main module for the LPMS_Utility program
//------------------------------------------------------------------------------
// History....: 28 June 2020 - Adapt from LPMS C++ version of LPMS_FirstRun
//------------------------------------------------------------------------------

unit LPMS_UtilityApp;

{$mode objfpc}{$H+}

interface
//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
   ExtCtrls, Buttons, DBGrids, DBCtrls, EditBtn, Spin, usplashabout,
   DateTimePicker, LazFileUtils, sqldb, db, LCLType, Math,

{$IFDEF WINDOWS}                     // Target is Winblows
   Registry, mysql56conn;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   IniFiles,
   {$IFDEF CPUARMHF}                 // Running on ARM (Raspbian) architecture
      mysql55conn;
   {$ELSE}                           // Running on Intel architecture
      mysql57conn;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                      // Target is macOS
   IniFiles,
  {$IFDEF CPUI386}                   // Running on old hardware i.e. i386 - Widget set must be Carbon
      mysql55conn;
   {$ELSE}                           // Running on x86_64 - Widget set must be Cocoa
      mysql57conn;
   {$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_UtilityApp }

   TFLPMS_UtilityApp = class(TForm)
      btnAll: TSpeedButton;
      btnAllB: TSpeedButton;
      btnArchive: TSpeedButton;
      btnCancelB: TButton;
      btnCancelC: TButton;
      btnCancelD: TButton;
      btnCancelF: TButton;
      btnCancelL: TButton;
      btnCancelM: TButton;
      btnCancelR: TButton;
      btnCancelU: TButton;
      btnFirst: TSpeedButton;
      btnGet: TButton;
      btnLast: TSpeedButton;
      btnLockB: TSpeedButton;
      btnLockC: TSpeedButton;
      btnLockD: TSpeedButton;
      btnLockL: TSpeedButton;
      btnLockS: TSpeedButton;
      btnLockU: TSpeedButton;
      btnNext: TSpeedButton;
      btnOpenLog: TButton;
      btnPrev: TSpeedButton;
      btnProcessB: TButton;
      btnProcessC: TButton;
      btnProcessD: TButton;
      btnProcessF: TButton;
      btnProcessL: TButton;
      btnProcessM: TButton;
      btnRefresh: TSpeedButton;
      btnRegister: TButton;
      btnReload: TSpeedButton;
      btnSearchBoth: TSpeedButton;
      btnSearchDesc: TSpeedButton;
      btnSearchUser: TSpeedButton;
      btnUnlockB: TSpeedButton;
      btnUnlockC: TSpeedButton;
      btnUnlockD: TSpeedButton;
      btnUnlockL: TSpeedButton;
      btnUnlockS: TSpeedButton;
      btnUnlockU: TSpeedButton;
      btnUpgradeU: TButton;
      cbDebug: TCheckBox;
      cbIgnore: TCheckBox;
      cbMultiCpy: TCheckBox;
      cbNewEncode: TCheckBox;
      cbNewPass: TCheckBox;
      cbPersist: TCheckBox;
      cbType: TCheckBox;
      chkAutoRefresh: TCheckBox;
      chkMatchAny: TCheckBox;
      Convert: TTabSheet;
      SQLDs1: TDataSource;
      DBGrid1: TDBGrid;
      DBNavigator1: TDBNavigator;
      dtpEDate: TDateTimePicker;
      dtpETime: TDateTimePicker;
      dtpSDate: TDateTimePicker;
      dtpSTime: TDateTimePicker;
      edtKeyM: TEditButton;
      edtCompany: TEdit;
      edtCpyName: TEdit;
      edtCurrVersion: TEdit;
      edtDate: TEdit;
      edtDateL: TEdit;
      edtDescriptionL: TEdit;
      edtEmail: TEdit;
      edtHostL: TEdit;
      edtHostName: TEdit;
      edtHostNameC: TEdit;
      edtHostNameD: TEdit;
      edtKey: TEdit;
      edtMode: TEdit;
      edtName: TEdit;
      edtNewPrefixC: TEdit;
      edtNewVersion: TEdit;
      edtNum: TEdit;
      edtPassword: TEdit;
      edtPasswordC: TEdit;
      edtPasswordD: TEdit;
      edtPasswordL: TEdit;
      edtPrefix: TEdit;
      edtPrefixC: TEdit;
      edtPrefixD: TEdit;
      edtPrefixF: TEdit;
      edtPrefixM: TEdit;
      edtRoot: TEdit;
      edtSearchDesc: TEdit;
      edtSearchUser: TEdit;
      edtSQLD: TComboBox;
      edtSubkey: TEdit;
      edtTime: TEdit;
      edtTimeL: TEdit;
      edtTitle: TEdit;
      edtType: TEdit;
      edtUnique: TEdit;
      edtUser: TEdit;
      edtUserL: TEdit;
      edtUserName: TEdit;
      edtUserNameC: TEdit;
      edtUserNameD: TEdit;
      edtVersion: TEdit;
      edtArchive: TFileNameEdit;
      edtBackupFile: TFileNameEdit;
      edtFolder: TFileNameEdit;
      edtSetupLoc: TFileNameEdit;
      edtSQLFile: TFileNameEdit;
      Image1: TImage;
      Image2: TImage;
      Image3: TImage;
      Image4: TImage;
      Image5: TImage;
      Image6: TImage;
      Image7: TImage;
      Image8: TImage;
      Label1: TLabel;
      Label10: TLabel;
      Label11: TLabel;
      Label12: TLabel;
      Label13: TLabel;
      Label14: TLabel;
      Label15: TLabel;
      Label16: TLabel;
      Label17: TLabel;
      Label18: TLabel;
      Label19: TLabel;
      Label2: TLabel;
      Label20: TLabel;
      Label21: TLabel;
      Label22: TLabel;
      Label23: TLabel;
      Label24: TLabel;
      Label25: TLabel;
      Label26: TLabel;
      Label27: TLabel;
      Label28: TLabel;
      Label29: TLabel;
      Label3: TLabel;
      Label30: TLabel;
      Label31: TLabel;
      Label32: TLabel;
      Label33: TLabel;
      Label34: TLabel;
      Label35: TLabel;
      Label36: TLabel;
      Label37: TLabel;
      Label38: TLabel;
      Label39: TLabel;
      Label4: TLabel;
      Label40: TLabel;
      Label41: TLabel;
      Label42: TLabel;
      Label43: TLabel;
      Label5: TLabel;
      Label56: TLabel;
      Label57: TLabel;
      Label6: TLabel;
      Label7: TLabel;
      Label76: TLabel;
      Label79: TLabel;
      Label8: TLabel;
      Label9: TLabel;
      lblDate: TLabel;
      lblMode: TLabel;
      lblTime: TLabel;
      lblTitle: TLabel;
      lblType: TLabel;
      lblVersion: TLabel;
      Log: TTabSheet;
      lvAlpha: TListView;
      lvExclude: TListView;
      lvLog: TListView;
      lvNumeric: TListView;
      lvTables: TListView;
      Maintenance: TTabSheet;
      PageControl1: TPageControl;
      pgTabs: TPageControl;
      rbFull: TRadioButton;
      rbPartial: TRadioButton;
      rbRestore: TRadioButton;
      rbSave: TRadioButton;
      rbSilent: TRadioButton;
      Register: TTabSheet;
      Restore: TTabSheet;
      Setup: TTabSheet;
      speInterval: TSpinEdit;
      saAbout: TSplashAbout;
      SQL: TTabSheet;
      SQLQry1: TSQLQuery;
      SQLTran: TSQLTransaction;
      StaticText1: TStaticText;
      StaticText2: TStaticText;
      StaticText3: TStaticText;
      StaticText4: TStaticText;
      StaticText5: TStaticText;
      StaticText6: TStaticText;
      stMsg: TStaticText;
      stMsgB: TStaticText;
      stMsgL: TStaticText;
      stProgress: TStaticText;
      stProgressB: TStaticText;
      Upgrade: TTabSheet;
      procedure btnCancelRClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure Image2Click(Sender: TObject);

private  { Private Declarations }
   ArchiveActive  : boolean;       //
   DateIsSet      : boolean;       //
   LockB_State    : boolean;       // Lock status of the Restore Tab
   LockC_State    : boolean;       // Lock status of the Convert Tab
   LockD_State    : boolean;       // Lock status of the SQL Tab
   LockS_State    : boolean;       // Lock status of the Setup Tab
   LockL_State    : boolean;       // Lock status of the Log Tab
   LockU_State    : boolean;       // Lock status of the Upgrade Tab
   SearchBoth     : boolean;       //
   SearchDesc     : boolean;       //
   SearchUser     : boolean;       //
   Selected       : boolean;       //
   Sema           : boolean;       //
   SemaSQL        : boolean;       //
   ClientContact  : string;        //
   ClientEmail    : string;        //
   ClientName     : string;        //
   ClientUnique   : string;        //
   ErrMsg         : string;        // Last error message
   FilesDir       : string;        //
   HostName       : string;        //
   KeepRegString  : string;        //
   LPMSUpgrade    : string;        // Path to the LPMS_Upgrade utility
   OSName         : string;        // Name of the OS we are running on
   OSShort        : string;        // Short name of the OS we are running on
   PassPhrase     : string;        // Used by Inout Query
   Password       : string;        //
   Prefix         : string;        //
   RegPath        : string;        // Path to the local INI file - Not used on Winblows
   RestoreTables  : string;        //
   SecretPhrase   : string;        // Used by Vignere
   ServerName     : string;        // The HostName where the LPMS_Server is running
   ServerPort     : string;        // The Port on which the LPMS_Server is listening
   SQLFile        : string;        //
   ThisGUID       : string;        //
   ThisInstall    : string;        //
   UserName       : string;        //
   UserKey        : string;        // The LPMS Key stored in the Registry
   LogList        : TStringList;   //
   ImportList     : TStringList;   //
   RespList       : TStringList;   //
   TableList      : TStringList;   //


{$IFDEF WINDOWS}                   // Target is Winblows
   SQLCon  : TMySQL56Connection;
{$ENDIF}

{$IFDEF LINUX}                     // Target is Linux
   {$IFDEF CPUARMHF}               // Running on ARM (Raspbian) architecture
      SQLCon : TMySQL55Connection;
   {$ELSE}                         // Running on Intel architecture
      SQLCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                    // Target is macOS
   {$IFDEF CPUI386}                // Running on old hardware i.e. i386 - Widget set must be Carbon
      SQLCon : TMySQL55Connection;
   {$ELSE}                         // Running on x86_64 - Widget set must be Cocoa
      SQLCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string;

public   { Publlic Declartions}
   DBPrefix       : string;        // The Database Prefix stored in the Registry
   DoRestore      : boolean;       //
   MultiCompany   : boolean;       //
   Proceed        : boolean;       //
   RestoreHost    : string;        //
   RestorePass    : string;        //
   RestoreUser    : string;        //
   Result         : string;        //
   ThisDBPrefix   : string;        //
   ThisPass       : string;        //

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
   CYPHER_ENC         = 0;
   CYPHER_DEC         = 1;

var
   FLPMS_UtilityApp: TFLPMS_UtilityApp;

implementation

{$R *.lfm}

{ TFLPMS_UtilityApp }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.FormCreate(Sender: TObject);
var
{$IFDEF WINDOWS}
   RegIni    : TRegistryIniFile;
{$ELSE}
   RegIni    : TINIFile;
{$ENDIF}

begin

//--- Set the Format Settings to override the system locale

   DefaultFormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   DefaultFormatSettings.LongTimeFormat    := 'HH:nn:ss';
   DefaultFormatSettings.DateSeparator     := '/';
   DefaultFormatSettings.ThousandSeparator := ' ';

   saAbout                 := TSplashAbout.Create(nil);
   saAbout.Author          := 'BlueCrane Software Development CC';
   saAbout.BackGroundColor := clSkyBlue;
   saAbout.UserTitle       := 'LPMS Utility';
   saAbout.Description     := 'Made with: LCL' + saAbout.PoweredBy.InfoLCLVersion + ' and FPC ' + saAbout.PoweredBy.InfoFPCVersion + #10 + 'For: ' + saAbout.PoweredBy.InfoFPCTarget + ' (' + saAbout.PoweredBy.InfoWidgetSet + ')' + #10 + #10 + 'Support: support@bluecrane.cc' + #10 + 'Visit: www.bluecrane.cc' + #10 + #10 + 'Copyright (c) 2009 - ' + FormatDateTime('yyyy',Now());
   saAbout.ShowDescription := True;

{$IFDEF WINDOWS}                    // Target is Winblows
   OSName  := 'MS-Windows';
   OSSHort := 'MS';
   sqlCon  := TMySQL56Connection.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
     OSName  := 'Raspbian';
     OSShort := 'Pi';
     sqlCon  := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
     OSName  := 'Linux';
     OSShort := 'L';
     sqlCon  := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   OSName  := 'macOS';
   OSShort := 'Mac';
   {$IFDEF CPUI386}                 // Running on older hardware .e.g. i386
     sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on newer hardwre e.g.x86_64
     sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

//--- We get the path to the user's home directory for storing the 'Registry'
//--- entries - this is platform independent and not used on Winblows as we
//--- us the proper Registry on Winblows.

{$IFNDEF WINDOWS}

   RegPath := AppendPathDelim(GetUSerDir);
   RegPath := AppendPathDelim(RegPath + '.lpms_utility');

//--- We now have what passes for a home directory with the working directory
//--- Backup Manager added to it and tests whether this exists. If it does not
//--- then we ask the User whether we should create it and do so if the User
//--- agrees otherwise we terminate the Application

   if DirectoryExists(RegPath) = False then begin

      if (Application.MessageBox('WARNING: LPMS Server directory does not exist. You can:' + #10 + #10 + #10 + 'Click [Yes] to create the directory; or' +#10 + #10 + 'Click [No] to terminate.','LPMS Utility',(MB_YESNO + MB_ICONWARNING)) = ID_NO) then begin

         Application.Terminate;
         Exit;

      end;

      if CreateDir(RegPath) = False then begin

         Application.MessageBox('FATAL: Unable to create LPMS Server directory.' + #10 + #10 + 'LPMS Server cannot continue and will be terminated.','LPMS Utility',(MB_OK + MB_ICONSTOP));
         Application.Terminate;
         Exit;

      end;

   end;

{$ENDIF}

//--- Set the Encode/Decode key

   SecretPhrase := 'BLUECRANE SOFTWARE DEVELOPMENT CC';
   PassPhrase   := 'BlueCrane Software Development CC';

//--- Get the default values stored in the Registry

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create('Software\BlueCrane Software\LPMS 3');
{$ELSE}
   RegIni := TINIFile.Create(RegPath + 'LPMS 3.ini');
{$ENDIF}


   edtSQLFile.Text := RegIni.ReadString('Preferences','SQLLocation',RegPath + 'LPMS_SQL.txt');
   UserKey         := RegIni.ReadString('Preferences','Key','');
   ServerName      := RegIni.ReadString('Preferences','KeyHost','');
   ServerPort      := RegIni.ReadString('Preferences','KeyPort','');
   LPMSUpgrade     := RegIni.ReadString('Preferences','LPMSUpgrade',AppendPathDelim(ExtractFilePath(Application.ExeName)) + 'LPMS_Upgrade.exe');
   DBPrefix        := RegIni.ReadString('Preferences','DBPrefix','invalid');

   RegIni.Destroy;

//--- Build the DB connection string


   SQLCon.HostName     := ServerName;
   SQLCon.UserName     := 'LPMSAdmin';
   SQLCon.Password     := 'LA01';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;
   SQLDs1.DataSet      := SQLQry1;


end;

//------------------------------------------------------------------------------
// User clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnCancelRClick(Sender: TObject);
begin

   Close;

end;

//------------------------------------------------------------------------------
// User clicked on the top right icon to show about information
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.Image2Click(Sender: TObject);
begin

   saAbout.ShowAbout;

end;

//---------------------------------------------------------------------------
// Function to do a Vignere Cypher
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.Vignere(ThisType: integer; Phrase: string; const Key: string) : string;
const
   OrdBigA = Ord('A');
   OrdBigZ = Ord('Z');
   OrdSmlA = Ord('a');
   OrdSmlZ = Ord('z');

var
   idx1, idx2, PThisChr, NThisChr, PhraseLen, ThisKeyLen : integer;
   Encrypted                                             : string;
   TempKey, NewPhrase                                    : WideString;

begin

//--- Remove all characters that do not fall within [A..Z] and [a..z]

   TempKey := '';

   for idx1 := 1 to Length(Key) do begin

      if ((InRange(Ord(Key[idx1]), OrdBigA, OrdBigZ)) or (InRange(Ord(Key[idx1]), OrdSmlA, OrdSmlZ))) = True then
         TempKey := TempKey + Key[idx1];

   end;

   PhraseLen  := Length(Phrase);
   ThisKeyLen := Length(TempKey);

//--- Now extend or limit the Key to the same length as the Phrase

   idx2   := 1;
   NewPhrase := '';

   for idx1 := 1 to PhraseLen do begin

      if idx2 > ThisKeyLen then
         idx2 := 1;

      NewPhrase := NewPhrase + TempKey[idx2];
      Inc(idx2);

   end;

//--- Do the Encryption or Decryption depending on the value of Type. Only
//--- characters between A-Z and a-z are transformed. The rest are left as is.

   Encrypted := '';

   case ThisType of

      CYPHER_ENC: begin

         for idx1 := 1 to PhraseLen do begin

            PThisChr := Ord(Phrase[idx1]);
            NThisChr := Ord(NewPhrase[idx1]);

            if ((PThisChr >= OrdBigA) and (PThisChr <= OrdBigZ)) then
               Encrypted := Encrypted + Chr(((PThisChr + NThisChr) mod 26) + OrdBigA)
            else if ((PThisChr >= OrdSmlA) and (PThisChr <= OrdSmlZ)) then
               Encrypted := Encrypted + Chr(((PThisChr + NThisChr) mod 26) + OrdSmlA)
            else
               Encrypted := Encrypted + Phrase[idx1];

         end;

      end;

      CYPHER_DEC: begin

         for idx1 := 1 to PhraseLen do begin

            PThisChr := Ord(Phrase[idx1]);
            NThisChr := Ord(NewPhrase[idx1]);

            if ((PThisChr >= OrdBigA) and (PThisChr <= OrdBigZ)) then
               Encrypted := Encrypted + Chr(((PThisChr - NThisChr + 26) mod 26) + OrdBigA)
            else if ((PThisChr >= OrdSmlA) and (PThisChr <= OrdSmlZ)) then
               Encrypted := Encrypted + Chr(((PThisChr - NThisChr + 14) mod 26) + OrdSmlA)
            else
               Encrypted := Encrypted + Phrase[idx1];

         end;

      end;

   end;

   Result := Encrypted;

end;

//------------------------------------------------------------------------------
end.

