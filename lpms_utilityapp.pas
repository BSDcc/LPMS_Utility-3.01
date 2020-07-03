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
   ExtCtrls, Buttons, DBGrids, DBCtrls, EditBtn, Spin, usplashabout, StrUtils,
   DateTimePicker, LazFileUtils, sqldb, db, LCLType, Math, DOM, Process,

{$IFDEF WINDOWS}                     // Target is Winblows
   Registry, Windows, mysql56conn;
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
      Bevel1: TBevel;
      Bevel2: TBevel;
      Bevel3: TBevel;
      Bevel4: TBevel;
      Bevel5: TBevel;
      Bevel6: TBevel;
      Bevel7: TBevel;
      Bevel8: TBevel;
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
      procedure btnLockDClick(Sender: TObject);
      procedure btnProcessDClick(Sender: TObject);
      procedure btnProcessMClick(Sender: TObject);
      procedure edtHostNameDChange(Sender: TObject);
      procedure edtKeyMButtonClick(Sender: TObject);
      procedure edtKeyMChange(Sender: TObject);
      procedure edtKeyMKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure edtPrefixMChange(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure Image2Click(Sender: TObject);
      procedure pgTabsChange(Sender: TObject);
      procedure SQLQry1AfterOpen(DataSet: TDataSet);

private  { Private Declarations }
   CallHWND       : integer;       //
   ArchiveActive  : boolean;       //
   CanUpdate      : boolean;       // Semaphore to inhibit update processing
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
   LPMSUpgrade    : string;        // Path to the LPMS_Upgrade utility
   OSName         : string;        // Name of the OS we are running on
   OSShort        : string;        // Short name of the OS we are running on
   Password       : string;        //
   Prefix         : string;        //
   RestoreTables  : string;        //
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
   ExportSet      : TDOMNode;      //
   TableSet       : TDOMNode;      //
   ThisNode       : TDOMNode;      //

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

   procedure SilentUpgrade();
   function  DM_Put_DB(S1: string; RunType: integer) : boolean;
   function  DM_Open_Connection() : boolean;
   function  InputQueryM(ThisCap, Question: string; DispType: integer) : string;

public   { Publlic Declartions}
   DoRestore      : boolean;       //
   MultiCompany   : boolean;       //
   Proceed        : boolean;       //
   DBPrefix       : string;        // The Database Prefix stored in the Registry
   KeepRegString  : string;        // Holds the actual Registry location depending on the platform and the value of MultiCompany
   PassPhrase     : string;        // Used by Input Query
   RegPath        : string;        // Path to the local INI file - Not used on Winblows
   RestoreHost    : string;        //
   RestorePass    : string;        //
   RestoreUser    : string;        //
   Result         : string;        //
   SecretPhrase   : string;        // Used by Vignere
   ThisDBPrefix   : string;        //
   ThisPass       : string;        //
   ThisRes        : string;        // Result from InputQuery

   function Vignere(ThisType: integer; Phrase: string; const Key: string) : string;

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
const
   BUFFER_LEN         = 1024;
   CYPHER_ENC         = 0;
   CYPHER_DEC         = 1;
   IM_UNREAD          = 1;
   LINE_TABLE         = 0;
   SERVER_DELIM       = '|';
   TYPE_READ          = 1;
   TYPE_WRITE         = 2;
   TYPE_PLAIN         = 1;
   TYPE_CODED         = 2;
   TYPE_SELECT        = 1;
   TYPE_OTHER         = 2;
   TYPE_PASSWORD      = 1;
   TYPE_TEXT          = 2;
   TYPE_ARCHIVE       = 1;
   TYPE_CURRENT       = 2;
   TYPE_MASK          = 1;
   TYPE_UNMASK        = 2;

type

   REC_Key_Priv = record
      Key              : string;
      DaysLeft         : integer;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
      DBPrefix         : string;
      Unique           : string;
      KeyDate          : string;
   end;

   REC_Key_Values = record
      Unique           : string;
      ExpDate          : string;
      DBPrefix         : string;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
   end;

//--- Temporary - Allow both old style encoding and new style encoding but
//--- only one type at a time

//{$DEFINE OLD_ENCODING}

var
   FLPMS_UtilityApp: TFLPMS_UtilityApp;

   This_Key_Values : REC_Key_Values;
   This_Key_Priv   : REC_Key_Priv;

{$IFDEF DARWIN}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; stdcall; external 'libbsd_utilities.dylib';
   function  MaskField(InputField: string; MaskType: integer): string; stdcall; external 'libbsd_utilities.dylib';
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.dylib';
{$ENDIF}
{$IFDEF WINDOWS}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; stdcall; external 'BSD_Utilities.dll';
   function  MaskField(InputField: string; MaskType: integer): string; stdcall; external 'BSD_Utilities.dll';
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'BSD_Utilities.dll';
{$ENDIF}
{$IFDEF LINUX}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; stdcall; external 'libbsd_utilities.so';
   function  MaskField(InputField: string; MaskType: integer): string; stdcall; external 'libbsd_utilities.so';
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.so';
{$ENDIF}

implementation

   uses LPMS_UtilityMultiCpy, LPMS_InputQuery;

{$R *.lfm}

{ TFLPMS_UtilityApp }

{==============================================================================}
{--- General functions                                                      ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.FormCreate(Sender: TObject);
var
   idx1, NumParms : integer;
   ParmsFound     : boolean = True;
   ThisPassPhrase : string;
   Params, Args   : TStringList;
{$IFDEF WINDOWS}
   RegIni         : TRegistryIniFile;
{$ELSE}
   RegIni         : TINIFile;
{$ENDIF}

begin

   CanUpdate := False;

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

//--- Checek whether any parameters to do a Silent install were passed

   //

//--- Check whether we are dealing with a Multi Company situation

   FLPMS_UtilityMultiCpy := TFLPMS_UtilityMultiCpy.Create(Application);

   FLPMS_UtilityApp.Hide();
   FLPMS_UtilityMultiCpy.ShowModal();
   FLPMS_UtilityApp.Show();

   FLPMS_UtilityMultiCpy.Destroy;

//--- Get the default values stored in the Registry

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
   RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}

   edtSQLFile.Text := RegIni.ReadString('Preferences','SQLLocation',RegPath + 'LPMS_SQL.txt');
   UserKey         := RegIni.ReadString('Preferences','Key','');
   ServerName      := RegIni.ReadString('Preferences','KeyHost','');
   ServerPort      := RegIni.ReadString('Preferences','KeyPort','');
   LPMSUpgrade     := RegIni.ReadString('Preferences','LPMSUpgrade',AppendPathDelim(ExtractFilePath(Application.ExeName)) + 'LPMS_Upgrade.exe');
   DBPrefix        := RegIni.ReadString('Preferences','DBPrefix','invalid');

   RegIni.Destroy;

//--- Check whether any paramters were passed and retrieve if so

   CallHWND := 0;

   try

      Params  := TStringList.Create;
      Args    := TStringList.Create;

      for idx1 := 1 to ParamCount do
         Args.Add(ParamStr(idx1));

//--- Call and execute the cmdlOptions function in the BSD_Utilities DLL

      NumParms := cmdlOptions('G:M:P:H:', Args, Params);

      if NumParms > 0 then begin

         idx1      := 0;
         NumParms := NumParms * 2;

         while idx1 < Params.Count do begin

            if Params.Strings[idx1] = 'G' then               // GUID for LPMS installation upgrade
               ThisGUID := Params.Strings[idx1 + 1];

            if Params.Strings[idx1] = 'M' then              // Full path to "setup.exe" for new version
               ThisInstall := Params.Strings[idx1 + 1];

            if Params.Strings[idx1] = 'P' then              // Passphrase to run FirstRun - required
               ThisPassPhrase := Params.Strings[idx1 + 1];

{$IFDEF WINDOWS}
            if Params.Strings[idx1] = 'H' then begin        // Hide and Show the callers Form

               CallHWND := StrToInt(Params.Strings[idx1 + 1]);

               if CallHWND <> 0 then
                  ShowWindow(CallHWND,SW_HIDE);

            end;
{$ENDIF}

            idx1 := idx1 + 2;

         end;

      end else
         ParmsFound := False;

   finally

//      Params.Destroy;
      Args.Free;

   end;

//--- Attempt a silent uninstall and upgrade of LPMS if both GUID and Setup
//--- location was passed as parameters

   if (ParmsFound = True) and ((ThisGUID <> '') and (ThisInstall <> '')) and (ThisPassPhrase = PassPhrase) then begin

      SilentUpgrade();
      Application.Terminate;
      Exit;

   end;

//--- If we get here then no/insufficient/invalid parameters were passed.
//--- Check whether this user is already registered

   if UserKey = '** Not Registered **' then begin

      pgTabs.Pages[0].Visible := True;
      pgTabs.Pages[1].Visible := False;
      pgTabs.Pages[2].Visible := False;
      pgTabs.Pages[3].Visible := False;
      pgTabs.Pages[4].Visible := False;
      pgTabs.ActivePageIndex  := 0;

   end else begin

      pgTabs.Pages[0].Visible := False;
      pgTabs.Pages[1].Visible := False;
      pgTabs.Pages[2].Visible := True;
      pgTabs.Pages[3].Visible := False;
      pgTabs.Pages[4].Visible := False;
      pgTabs.ActivePageIndex  := 2;

   end;

//--- Display the landing tab

   pgTabsChange(Sender);

//--- Set some default values

   btnRegister.Enabled := False;
   btnProcessF.Enabled := False;
   btnProcessM.Enabled := False;
   btnUpgradeU.Enabled := False;

{
//--- Retrieve this PC's Mac Addresses (Up to 6)

   This_Key_Rec  = new LPMS_Key_Rec;
   DldDecode->LPMS_Key_GetUnique(This_Key_Rec);

   if (This_Key_Rec->MacAddress[0] == "")
   {
      MessageBox(NULL,"Unable to find a valid MACAddress - FirstRun cannot continue...","Legal Practise Management System - FirstRun",(MB_OK|MB_ICONSTOP|MB_SYSTEMMODAL));
      delete This_Key_Rec;
      Application->Terminate();
      return;
   }
   else
      edtUnique->Text = This_Key_Rec->MacAddress[0];

   delete This_Key_Rec;
}

end;

//------------------------------------------------------------------------------
// Executed when the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.FormShow(Sender: TObject);
begin

   //

end;

//------------------------------------------------------------------------------
// User changed to a different Tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.pgTabsChange(Sender: TObject);
const
   NUM_VAL = 48;
   TAB_0   = 0;
   TAB_1   = 1;
   TAB_2   = 2;
   TAB_3   = 3;
   TAB_4   = 4;
   TAB_SQL   = 5;
   TAB_6   = 6;
   TAB_7   = 7;

var
   idx          : integer;
   ThisPrefix : string;
   ThisList     : TListItem;

begin

//--- Reset the information on the Setup tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_1 then
      LockS_State := True;

//--- Reset the information on the Convert tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_3 then begin

      edtCurrVersion.Clear();

      LockC_State         := True;
      stProgress.Caption  := '';
      StaticText3.Caption := 'LPMS Utility: Enter the information below then click on the [Get] button to retrieve the current Data Base version';

   end;

//--- Reset the information on the Upgrade tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_4 then begin

      lvAlpha.Clear();
      lvNumeric.Clear();
      lvExclude.Clear();
      edtFolder.Clear();
      edtRoot.Clear();
      edtSubkey.Clear();
      edtSetupLoc.Clear();

      LockU_State       := True;
      rbSilent.Checked  := False;
      rbSave.Checked    := False;
      rbRestore.Checked := False;
      cbPersist.Checked := False;
      cbIgnore.Checked  := False;
      cbDebug.Checked   := False;

   end;

//--- Reset the information on the SQL tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_SQL then begin

      LockD_State         := True;
      StaticText6.Caption := 'LPMS Utility: Enter the database information below together with a valid SQL statement then click on ''Process''';

   end;

//--- Reset the information on the Restore tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_6 then begin

      edtBackupFile.Clear();
      edtTitle.Clear();
      edtDate.Clear();
      edtMode.Clear();
      edtVersion.Clear();
      edtTime.Clear();
      edtType.Clear();

      LockB_State            := True;
      edtBackupFile.ReadOnly := True;
      rbFull.Checked         := False;
      rbPartial.Checked      := False;
      cbType.Checked         := False;
      stMsgB.Caption         := 'LPMS Utility: Enter or select the backup file to restore then select the required options before clicking on ''Restore''';

   end;

//--- Reset the information on the Log Display tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_7 then begin

      edtUserL.Clear();
      edtPasswordL.Clear();
      edtHostL.Clear();
      edtArchive.Clear();
      edtSearchUser.Clear();
      edtSearchDesc.Clear();
      edtUser.Clear();
      edtDateL.Clear();
      edtTimeL.Clear();
      edtDescriptionL.Clear();
      lvLog.Clear();

      LockL_State            := True;
      chkAutoRefresh.Checked := False;
      chkMatchAny.Checked    := False;
      speInterval.Value      := 60;
      stMsgL.Caption         := 'LPMS Utility: Provide a valid ''User'', ''Password'' and ''Host'' then click on ''Current Log'' or select a Log Archive then click on ''Load''' + #10 + 'to display the contents';

   end;

//--- Set up the tab selected by the user

{$IFDEF OLD_ENCODING}
   ThisPrefix := jvCipher.DecodeString(SecretPhrase,Copy(DBPrefix,1,6));
{$ELSE}
   ThisPrefix := Vignere(CYPHER_DEC,Copy(DBPrefix,1,6),SecretPhrase);
{$ENDIF}

   if pgTabs.ActivePageIndex = TAB_0 then begin

      edtKey.Text    := UserKey;
      edtPrefix.Text := ThisPrefix;

//      edtNameChange(Sender);

      btnRegister.Default := True;
      btnCancelR.Caption  := 'Cancel';
      btnCancelR.Default  := False;
      edtName.SetFocus();

   end else if pgTabs.ActivePageIndex = TAB_1 then begin

      edtPrefixF.Text := ThisPrefix;

      if LockS_State = True then begin

         btnLockS.Visible    := True;
         btnUnlockS.Visible  := False;

         edtHostName.Enabled := False;
         edtUserName.Enabled := False;
         edtPassword.Enabled := False;
         edtSQLFile.Enabled  := False;
         edtCpyName.Enabled  := False;
         edtPrefixF.Enabled  := False;

         btnProcessF.Enabled := False;
         btnProcessF.Default := False;
         btnCancelF.Caption  := 'Cancel';
         btnCancelF.Default  := True;

      end else begin

         btnLockS.Visible    := False;
         btnUnlockS.Visible  := True;

         edtHostName.Enabled := True;
         edtUserName.Enabled := True;
         edtPassword.Enabled := True;
         edtSQLFile.Enabled  := True;
         edtCpyName.Enabled  := True;
         edtPrefixF.Enabled  := True;

//         edtHostNameChange(Sender);

         btnProcessF.Default := True;
         btnCancelF.Caption  := 'Cancel';
         btnCancelF.Default  := False;

         edtHostName.SetFocus();

      end;

   end else if pgTabs.ActivePageIndex = TAB_2 then begin

      edtPrefixM.Text := ThisPrefix;

      edtKeyM.Text        := UserKey;
      btnProcessM.Enabled := False;

      edtPrefixMChange(Sender);

      btnProcessM.Default := True;
      btnCancelM.Caption  := 'Cancel';
      btnCancelM.Default  := False;

      edtPrefixM.SetFocus();

   end else if pgTabs.ActivePageIndex = TAB_3 then begin

      edtPrefixC.Text    := ThisPrefix;
      edtNewVersion.Text := '3.2.1';
      stProgress.Caption := '';

      if LockC_State = True then begin

         btnLockC.Visible       := True;
         btnUnlockC.Visible     := False;

         edtHostNameC.Enabled   := False;
         edtUserNameC.Enabled   := False;
         edtPasswordC.Enabled   := False;
         edtPrefixC.Enabled     := False;
         edtCurrVersion.Enabled := False;
         edtNewVersion.Enabled  := False;

         btnGet.Enabled         := False;

         btnProcessC.Enabled    := False;
         btnProcessC.Default    := False;
         btnCancelC.Caption     := 'Cancel';
         btnCancelC.Default     := True;

      end else begin

         btnLockC.Visible       := False;
         btnUnlockC.Visible     := True;

         edtHostNameC.Enabled   := True;
         edtUserNameC.Enabled   := True;
         edtPasswordC.Enabled   := True;
         edtPrefixC.Enabled     := True;
         edtCurrVersion.Enabled := True;
         edtNewVersion.Enabled  := True;

         btnGet.Enabled         := True;
//         edtHostNameCChange(Sender);

         btnProcessC.Default    := True;
         btnCancelC.Caption     := 'Cancel';
         btnCancelC.Default     := False;

         edtHostNameC.SetFocus();

      end;

   end else if pgTabs.ActivePageIndex = TAB_4 then begin

      cbPersist.Checked   := False;
      cbIgnore.Checked    := False;
      cbDebug.Checked     := False;

      btnUpgradeU.Enabled := False;

      if LockU_State = True then begin

         btnLockU.Visible   := True;
         btnUnlockU.Visible := False;

         lvAlpha.Enabled     := False;
         lvNumeric.Enabled   := False;
         lvExclude.Enabled   := False;
         rbSilent.Enabled    := False;
         rbSave.Enabled      := False;
         rbRestore.Enabled   := False;
         cbPersist.Enabled   := False;
         cbIgnore.Enabled    := False;
         cbDebug.Enabled     := False;
         edtFolder.Enabled   := False;
         edtRoot.Enabled     := False;
         edtSubkey.Enabled   := False;
         edtSetupLoc.Enabled := False;

         btnUpgradeU.Enabled := False;
         btnUpgradeU.Default := False;
         btnCancelU.Caption  := 'Cancel';
         btnCancelU.Default  := True;

      end else begin

         btnLockU.Visible   := False;
         btnUnlockU.Visible := True;

         lvAlpha.Enabled     := True;
         lvNumeric.Enabled   := True;
         lvExclude.Enabled   := True;
         rbSilent.Enabled    := True;
         rbSave.Enabled      := True;
         rbRestore.Enabled   := True;
         cbPersist.Enabled   := True;
         cbIgnore.Enabled    := True;
         cbDebug.Enabled     := True;
         edtFolder.Enabled   := True;
         edtRoot.Enabled     := True;
         edtSubkey.Enabled   := True;
         edtSetupLoc.Enabled := True;

         btnUpgradeU.Default := True;
         btnCancelU.Caption  := 'Cancel';
         btnCancelU.Default  := False;

         for idx := 0 to NUM_VAL do begin

            ThisList := lvAlpha.Items.Add();
            ThisList.Caption := '';

            ThisList := lvNumeric.Items.Add();
            ThisList.Caption := '';

            ThisList := lvExclude.Items.Add();
            ThisList.Caption := '';

         end;

         edtFolder.Text := RegPath;
         edtSubkey.Text := 'Preferences';
{$IFDEF WINDOWS}
         edtRoot.Text   := 'Software\\BlueCrane Software\\LPMS 3';
{$ELSE}
         edtRoot.Text   := 'LPMS 3.ini';
{$ENDIF}

      end;

   end else if pgTabs.ActivePageIndex = TAB_SQL then begin

      SQLQry1.Close();
      btnProcessD.Enabled := False;

      if LockD_State = True then begin

         btnLockD.Visible     := True;
         btnUnlockD.Visible   := False;

         edtHostNameD.Enabled := False;
         edtUserNameD.Enabled := False;
         edtPasswordD.Enabled := False;
         edtPrefixD.Enabled   := False;
         edtSQLD.Enabled      := False;

         btnProcessD.Enabled := False;
         btnProcessD.Default := False;
         btnCancelD.Caption  := 'Cancel';
         btnCancelD.Default  := True;

      end else begin

         btnLockD.Visible     := False;
         btnUnlockD.Visible   := True;

         edtHostNameD.Enabled := True;
         edtUserNameD.Enabled := True;
         edtPasswordD.Enabled := True;
         edtPrefixD.Enabled   := True;
         edtSQLD.Enabled      := True;

         btnProcessD.Default := True;
         btnCancelD.Caption  := 'Cancel';
         btnCancelD.Default  := False;

         edtHostNameD.SetFocus();

      end;

   end else if pgTabs.ActivePageIndex = TAB_6 then begin

      rbFull.Checked      := True;
      cbType.Checked      := False;
      btnProcessB.Enabled := False;
      btnAllB.Enabled     := False;
      lvTables.Enabled    := False;
      Selected            := True;

      if LockB_State = True then begin

         btnLockB.Visible      := True;
         btnUnlockB.Visible    := False;
         btnAllB.Enabled       := False;

         edtBackupFile.Enabled := False;
//         btnOpenB.Enabled      := False;
         btnAllB.Enabled       := False;
         edtTitle.Enabled      := False;
         edtDate.Enabled       := False;
         edtTime.Enabled       := False;
         edtVersion.Enabled    := False;
         edtMode.Enabled       := False;
         edtType.Enabled       := False;
         rbFull.Enabled        := False;
         rbPartial.Enabled     := False;
         cbType.Enabled        := False;
         rbFull.Checked        := False;
         rbPartial.Checked     := False;
         cbType.Checked        := False;

         btnProcessB.Enabled   := False;
         btnProcessB.Default   := False;
         btnCancelB.Caption    := 'Cancel';
         btnCancelB.Default    := True;

      end else begin

         btnLockB.Visible      := False;
         btnUnlockB.Visible    := True;

         edtBackupFile.Enabled := True;
//         btnOpenB.Enabled      := True;

         edtBackupFile.SetFocus();

      end;

   end else if pgTabs.ActivePageIndex = TAB_7 then begin

      edtArchive.Clear();
      edtSearchUser.Clear();
      edtSearchDesc.Clear();
      edtUser.Clear();
      edtDateL.Clear();
      edtTimeL.Clear();
      edtDescriptionL.Clear();
      lvLog.Clear();
      chkMatchAny.Checked    := False;
      stMsg.Caption          := '0 Records';
      chkAutoRefresh.Checked := False;
      speInterval.Value      := 60;
      btnOpenLog.Enabled     := False;

      btnLockL.Visible       := True;
      btnUnlockL.Visible     := False;

      dtpSDate.Enabled       := False;
      dtpSTime.Enabled       := False;
      dtpEDate.Enabled       := False;
      dtpETime.Enabled       := False;
      btnReload.Enabled      := False;

      edtSearchUser.Enabled  := False;
      btnSearchUser.Enabled  := False;
      edtSearchDesc.Enabled  := False;
      btnSearchDesc.Enabled  := False;
      btnSearchBoth.Enabled  := False;
      chkMatchAny.Enabled    := False;

      btnFirst.Enabled       := False;
      btnPrev.Enabled        := False;
      btnNext.Enabled        := False;
      btnLast.Enabled        := False;

      lvLog.Enabled          := False;
      chkAutoRefresh.Enabled := False;
      speInterval.Enabled    := False;

      btnProcessL.Enabled    := False;

      btnAll.Enabled         := False;
      btnArchive.Enabled     := False;

      DateIsSet              := False;
      ArchiveActive          := False;

      edtUserL.SetFocus();

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Cancel button on a Tab
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

{==============================================================================}
{--- Maintenance Tab functions                                              ---}
{==============================================================================}

//------------------------------------------------------------------------------
// The Prefix changed on the Maintenance Page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtPrefixMChange(Sender: TObject);
begin

   if ((Trim(edtPrefixM.Text) = '') or (Trim(edtKeyM.Text) = '')) then
      btnProcessM.Enabled := False
   else
      btnProcessM.Enabled := True;

end;

//------------------------------------------------------------------------------
// User clicked on he button to clear the Key field
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtKeyMButtonClick(Sender: TObject);
begin

   edtKeyM.Clear;
   edtKeyM.SetFocus;

end;

//------------------------------------------------------------------------------
// The value of the Key field changed
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtKeyMChange(Sender: TObject);
var
   idx1, idx2, SelStrt : integer;
   DoIns               : boolean;
   ThisKey, ThisStr    : string;
   Parts               : TStringList;

begin

   if CanUpdate = True then
      Exit;

   if Trim(edtKeyM.Text) = '' then
      Exit;

   try

      Parts := TStringList.Create;

//--- Determine where the cursor is and whether we are at the end of the field.
//--- If we are not at the end then we are doing an insert

      SelStrt := edtKeyM.SelStart;

      if SelStrt < Length(edtKeyM.Text) then
         DoIns := True
      else
         DoIns := False;

      ThisKey  := '';
      ThisStr  := '';

//--- Remove the '-' characters from the key as these will shift due to new
//--- characters being added or eisting characters being deleted

      ExtractStrings(['-'], [' '], PChar(edtKeyM.Text), Parts);

      for idx1 := 0 to Parts.Count - 1 do
         ThisStr := ThisStr + Parts[idx1];

//--- Rebuild the structure of the key inserting '-' at the appropriate places

      for idx2 := 1 to Length(ThisStr) do begin

         ThisKey := ThisKey + ThisStr[idx2];

         if idx2 in [4,7,11,15,19,23,27] then
            ThisKey := ThisKey + '-';

      end;

   finally

      Parts.Free;

   end;

   CanUpdate    := True;
   edtKeyM.Text := ThisKey;

//--- If the cursor is positioned at a '-' then we need to move it forward by
//--- 1 position, however this test will fail if we are at the beginning of the
//--- field

   if SelStrt > 0 then begin

      if ThisKey[SelStrt] = '-' then
         Inc(SelStrt);

   end;

//--- Reposition the cursor depending on wheter we are doing an insert or not

   if DoIns = True then
      edtKeyM.SelStart := SelStrt
   else
      edtKeyM.SelStart := Length(edtKeyM.Text);

   CanUpdate := False;

   edtPrefixMChange(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the Update button on the Maintenance Page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnProcessMClick(Sender: TObject);
var
   idx1                     : integer;
   PartValid                : boolean;
   PartOne, PartTwo, T1, T2 : string;
{$IFDEF WINDOWS}
   RegIni                   : TRegistryIniFile;
{$ELSE}
   RegIni                   : TINIFile;
{$ENDIF}

begin

   if Length(edtPrefixM.Text) <> 6 then begin

      Application.MessageBox('Prefix is invalid. A valid Prefix is 6 characters in length and consists of 3 Alphabetic characters followed by 3 Numeric characters. Please provide a valid Prefix','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtPrefixM.SetFocus();
      Exit;

   end;

   PartOne   := Copy(edtPrefixM.Text,1,3);
   PartTwo   := Copy(edtPrefixM.Text,4,3);
   PartValid := True;

//--- Check that the first three characters are all alphabetic

   for idx1 := 1 to 3 do begin

      if (PartOne[idx1] in ['A'..'Z','a'..'z']) = False then begin
         PartValid := False;
         break;
      end;

   end;

//--- Cheek that the last three characters are all numeric

   for idx1 := 1 to 3 do begin

      if (PartTwo[idx1] in ['0'..'9']) = False then begin
         PartValid := False;
         break;
      end;

   end;

   if PartValid = False then begin

      Application.MessageBox('Prefix is invalid. A valid Prefix is 6 characters in length and consists of 3 Alphabetic characters followed by 3 Numeric characters. Please provide a valid Prefix','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtPrefixM.SetFocus();
      Exit;

   end;

   if Length(Trim(edtKeyM.Text)) <> 38 then begin

      Application.MessageBox('Key is a required field and must be exactly 38 characters long - Please provide a valid Key','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtKeyM.SetFocus();
      Exit;

   end;

{$IFDEF OLD_ENCODING}
   DBPrefix := jvCipher.EncodeString(ThisPass,(edtPrefixM.Text + FormatDateTime('yyyy/mm/dd',Now())));
{$ELSE}
   DBPrefix := Vignere(CYPHER_ENC,PChar(edtPrefixM.Text + FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now)),SecretPhrase);
   T1       := Copy(DBPrefix,1,6);
   T2       := Copy(DBPrefix,7,99);
   DBPrefix := T1 + MaskField(T2,TYPE_MASK);
{$ENDIF}

   UserKey  := edtKeyM.Text;

//--- Write the new values to the Registry

{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
   RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}

   RegIni.WriteString('Preferences','Key',UserKey);
   RegIni.WriteString('Preferences','DBPrefix',DBPrefix);

   RegIni.Destroy;

   Application.MessageBox('Prefix and Key sucessfully updated...','LPMS Utility',(MB_OK + MB_ICONINFORMATION));

   btnProcessM.Default := False;
   btnCancelM.Caption  := 'Close';
   btnCancelM.Default  := True;
   btnCancelM.SetFocus();

end;

//------------------------------------------------------------------------------
// Check whether the User pressed the backspace key while editing the Key on
// the Maintenance Tab to preserve the position of the '-' characters
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtKeyMKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   KeepSelStart, KeepLength : integer;
   ThisField                : string;

begin

//--- Trap and Process the Backspace key

   if Key = VK_BACK then begin

//--- If we are at the start of the field then we consume the key and do nothing

      if (edtKeyM.SelStart = 0) and (edtKeyM.SelLength = 0) then begin

         Key := 0;
         Exit;

      end;

//--- If the whole field is selected then simply delete the contents

      if edtKeyM.SelLength = Length(edtKeyM.Text) then begin

         edtKeyM.Text := '';
         KeepSelStart := 0;

      end else begin

//--- Otherwise delete what is selected

         ThisField    := '';
         KeepLength   := edtKeyM.SelLength;
         KeepSelStart := edtKeyM.SelStart;

         if KeepSelStart = 1 then

            ThisField := Copy(edtKeyM.Text,2,Length(edtKeyM.Text) - 1)

         else begin

            if KeepLength = 0 then
               ThisField := Copy(edtKeyM.Text,1,KeepSelStart - 1)
            else
               ThisField := Copy(edtKeyM.Text,1,KeepSelStart);

            ThisField := Thisfield + Copy(edtKeyM.Text,KeepSelStart + KeepLength + 1,Length(edtKeyM.Text) - 1);

         end;

//--- Update the field without invoking the edtKeyRChange routine

         CanUpdate := True;
         edtKeyM.Text := ThisField;
         CanUpdate := False;

      end;

      if KeepLength = 0 then
         edtKeyM.SelStart := KeepSelStart - 1
      else
         edtKeyM.SelStart := KeepSelStart;

      Key := 0;

   end;

end;

{==============================================================================}
{--- Upgrade Tab functions                                                  ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Parameters to perform a silent upgrade of LPMS were passed to LPMS_Utility
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.SilentUpgrade();
var
   idx      : integer;
   Process  : TProcess;

begin

   Process := TProcess.Create(nil);

   try

      Process.InheritHandles := False;
      Process.Options        := [poWaitOnExit];
      Process.ShowWindow     := swoShow;

//--- Copy default environment variables including DISPLAY variable for GUI
//--- application to work

      for idx := 1 to GetEnvironmentVariableCount do
         Process.Environment.Add(GetEnvironmentString(idx));

      Process.Executable := LPMSUpgrade;
      Process.Parameters.Add('--args');
      Process.Parameters.Add('-TFull');
      Process.Parameters.Add('-G' + ThisGUID);
      Process.Parameters.Add('-M' + AnsiReplaceStr(AnsiReplaceStr(ThisInstall,':','~'),' ','#'));
      Process.Parameters.Add('-F' + AnsiReplaceStr(AnsiReplaceStr(RegPath,':','~'),' ','#'));
      Process.Parameters.Add('-R' + AnsiReplaceStr(AnsiReplaceStr('Software\BlueCrane Software\LPMS 3',':','~'),' ','#'));
      Process.Parameters.Add('-SPreferences');

      FLPMS_UtilityApp.Hide();
      Process.Execute;
      FLPMS_UtilityApp.Show();

   finally
      Process.Free;
   end;

end;

{==============================================================================}
{--- SQL Tab functions                                                      ---}
{==============================================================================}

//------------------------------------------------------------------------------
// A field on the SQL Page changed
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtHostNameDChange(Sender: TObject);
var
   FldCount : integer = 0;

begin

   if LockD_State = True then
      Exit;

//--- Check if the Process button can be enabled

   if Trim(edtHostNameD.Text) <> '' then Inc(FldCount);
   if Trim(edtUserNameD.Text) <> '' then Inc(FldCount);
   if Trim(edtPasswordD.Text) <> '' then Inc(FldCount);
   if Trim(edtPrefixD.Text)   <> '' then Inc(FldCount);
   if Trim(edtSQLD.Text)      <> '' then Inc(FldCount);

   if FldCount = 5 then
      btnProcessD.Enabled := True
   else
      btnProcessD.Enabled := False;

end;

//------------------------------------------------------------------------------
// User clicked on the Lock button on the SQL Page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnLockDClick(Sender: TObject);
var
   Passwd : string;

begin

   if LockD_State = True then begin

      Passwd := InputQueryM('LPMS Utility','Pass phrase:',ord(TYPE_PASSWORD));

      if Passwd = PassPhrase then
         LockD_State := False;

   end else begin

      LockD_State := True;

      edtHostNameD.Clear();
      edtUserNameD.Clear();
      edtPasswordD.Clear();
      edtPrefixD.Clear();
      edtSQLD.Clear();

   end;

   pgTabsChange(Sender);

end;

//---------------------------------------------------------------------------
// User click on the Process button on the SQL Page
//---------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnProcessDClick(Sender: TObject);
var
   S1 : string;

begin

//--- Keep track of the commands that are entered

   if edtSQLD.Items.IndexOf(edtSQLD.Text) = -1 then
      edtSQLD.Items.Insert(0,edtSQLD.Text);

//--- Build the DB connection string

   SQLCon.HostName     := edtHostNameD.Text;
   SQLCon.UserName     := edtUserNameD.Text;
   SQLCon.Password     := edtPasswordD.Text;
   SQLCon.DatabaseName := edtPrefixD.Text + '_LPMS';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;
   SQLDs1.DataSet      := SQLQry1;

//--- Open a connection to the datastore named in HostName

   if DM_Open_Connection() = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Select the Database to use

   S1 := 'USE ' + edtPrefixD.Text + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   SemaSQL := False;

//--- Execute the SQL Statement

   SQLQry1.Close();
   SQLQry1.SQL.Text := edtSQLD.Text;

   if ((LowerCase(Copy(edtSQLD.Text,1,6)) = 'select') or (LowerCase(Copy(edtSQLD.Text,1,4)) = 'show')) then
      SQLQry1.Open()
   else
      SQLQry1.ExecSQL();

end;

//------------------------------------------------------------------------------
// Executed after data was loaded from the DB - adjust column width
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.SQLQry1AfterOpen(DataSet: TDataSet);
var
   idx1, idx2, Len, ThisTop{, ThisLen} : integer;
   Str                               : string;

begin

//   ThisLen := DBGrid1.Columns.Count;

   for idx1 := 0 to DBGrid1.Columns.Count - 1 do begin

      Str := DBGrid1.Columns.Items[idx1].Title.Caption;
      ThisTop := Length(Str) * 8;

      SQLQry1.First;

      for idx2 := 0 to SQLQry1.RecordCount - 1 do begin


         Str := SQLQry1.Fields.FieldByNumber(idx1 + 1).AsString;
         Len := Length(Str) * 8;

         if Len > ThisTop then
            ThisTop := Len;

         SQLQry1.Next;
         DBGrid1.Columns.Items[idx1].Width := ThisTop;

      end;

   end;

end;

{==============================================================================}
{--- Database functions                                                     ---}
{==============================================================================}

//---------------------------------------------------------------------------
// Function to open a connection to the datastore
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.DM_Open_Connection() : boolean;
begin

   try

      SQLQry1.Close;
      SQLCon.Open;

      except on E : Exception do begin

         ErrMsg := E.Message;
         Result := False;
         Exit;

      end;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Function to access the Database
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.DM_Put_DB(S1: string; RunType: integer) : boolean;
begin

   try

      SQLQry1.Close();
      SQLQry1.SQL.Text := S1;

      if RunType = TYPE_SELECT then
         SQLQry1.Open
      else
         SQLQry1.ExecSQL;

      except on E : Exception do begin

         ErrMsg := E.Message;

         if pgTabs.Pages[3].Visible = True then
            stProgress.Caption := '';

         Result := False;
         Exit;

      end;

   end;

   Result := True;

end;

{==============================================================================}
{--- Support functions                                                      ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Function to Request a PassPhrase from the User
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.InputQueryM(ThisCap, Question: string; DispType: integer) : string;
begin

   FLPMS_InputQuery := TFLPMS_InputQuery.Create(Application);

   FLPMS_InputQuery.Caption := ThisCap;
   FLPMS_InputQuery.edtInput.Clear();
   FLPMS_InputQuery.lblCaption.Caption := Question;

   if DispType = ord(TYPE_PASSWORD) then
      FLPMS_InputQuery.edtInput.PasswordChar := '*';

   FLPMS_UtilityApp.Hide();
   FLPMS_InputQuery.ShowModal();
   FLPMS_UtilityApp.Show();

   FLPMS_InputQuery.Destroy;

   Result := ThisRes;

end;

//------------------------------------------------------------------------------
// Function to do a Vignere Cypher
//------------------------------------------------------------------------------
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

