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
   IdTCPClient, StrUtils, DateTimePicker, LazFileUtils, sqldb, db, LCLType,
   Process,

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
      cbNewPass: TCheckBox;
      cbPersist: TCheckBox;
      cbType: TCheckBox;
      chkAutoRefresh: TCheckBox;
      chkMatchAny: TCheckBox;
      Convert: TTabSheet;
      stMsgL: TLabel;
      sdArchive: TSaveDialog;
      stMsgB: TLabel;
      tcpClient: TIdTCPClient;
      saAbout: TSplashAbout;
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
      imgMaintenance: TImage;
      imgSetup: TImage;
      imgRegister: TImage;
      imgConvert: TImage;
      imgUpgrade: TImage;
      imgSQL: TImage;
      imgRestore: TImage;
      imgLog: TImage;
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
      stProgress: TStaticText;
      stProgressB: TStaticText;
      timTimer: TTimer;
      Upgrade: TTabSheet;
      procedure btnAllBClick(Sender: TObject);
      procedure btnAllClick(Sender: TObject);
      procedure btnArchiveClick(Sender: TObject);
      procedure btnCancelRClick(Sender: TObject);
      procedure btnFirstClick(Sender: TObject);
      procedure btnGetClick(Sender: TObject);
      procedure btnLastClick(Sender: TObject);
      procedure btnLockDClick(Sender: TObject);
      procedure btnLockSClick(Sender: TObject);
      procedure btnNextClick(Sender: TObject);
      procedure btnOpenLogClick(Sender: TObject);
      procedure btnPrevClick(Sender: TObject);
      procedure btnProcessBClick(Sender: TObject);
      procedure btnProcessCClick(Sender: TObject);
      procedure btnProcessDClick(Sender: TObject);
      procedure btnProcessFClick(Sender: TObject);
      procedure btnProcessLClick(Sender: TObject);
      procedure btnProcessMClick(Sender: TObject);
      procedure btnRegisterClick(Sender: TObject);
      procedure btnReloadClick(Sender: TObject);
      procedure btnSearchBothClick(Sender: TObject);
      procedure btnSearchDescClick(Sender: TObject);
      procedure btnSearchUserClick(Sender: TObject);
      procedure btnUnlockBClick(Sender: TObject);
      procedure btnUnlockCClick(Sender: TObject);
      procedure btnUnlockLClick(Sender: TObject);
      procedure chkAutoRefreshClick(Sender: TObject);
      procedure edtArchiveAcceptFileName(Sender: TObject; var Value: String);
      procedure edtArchiveButtonClick(Sender: TObject);
      procedure edtArchiveChange(Sender: TObject);
      procedure edtBackupFileButtonClick(Sender: TObject);
      procedure edtBackupFileChange(Sender: TObject);
      procedure edtHostNameCChange(Sender: TObject);
      procedure edtHostNameChange(Sender: TObject);
      procedure edtHostNameDChange(Sender: TObject);
      procedure edtKeyMButtonClick(Sender: TObject);
      procedure edtKeyMChange(Sender: TObject);
      procedure edtKeyMKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure edtNameChange(Sender: TObject);
      procedure edtPrefixMChange(Sender: TObject);
      procedure edtSQLFileButtonClick(Sender: TObject);
      procedure edtUserLChange(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure imgRegisterClick(Sender: TObject);
      procedure lvLogClick(Sender: TObject);
      procedure lvTablesItemChecked(Sender: TObject; Item: TListItem);
      procedure pgTabsChange(Sender: TObject);
      procedure rbFullClick(Sender: TObject);
      procedure rbPartialClick(Sender: TObject);
      procedure speIntervalChange(Sender: TObject);
      procedure SQLQry1AfterOpen(DataSet: TDataSet);
      procedure timTimerTimer(Sender: TObject);

private  { Private Declarations }
{$IFDEF WINDOWS}
   CallHWND       : integer;       // If <> 0 then it contains the calling program's Windows Handle so that it can be temporarily hidden
{$ENDIF}
   ArchiveActive  : boolean;       // Used by the Log function
   CanUpdate      : boolean;       // Semaphore to inhibit update processing
   DateIsSet      : boolean;       // Used by the Log function
   LockB_State    : boolean;       // Lock status of the Restore Tab
   LockC_State    : boolean;       // Lock status of the Convert Tab
   LockD_State    : boolean;       // Lock status of the SQL Tab
   LockS_State    : boolean;       // Lock status of the Setup Tab
   LockL_State    : boolean;       // Lock status of the Log Tab
   LockU_State    : boolean;       // Lock status of the Upgrade Tab
   RestoreActive  : boolean;       // Semaphore to prevent user from clicking buttons or changing Tabs while a Restore is active
   SearchBoth     : boolean;       // Used on the Log Tab to determine whether either Desc or User or both Desc and User must be searched depending on whwether chkMatchAny is checked or not
   SearchDesc     : boolean;       // Used on the Log Tab to seacrh only Description
   SearchUser     : boolean;       // Used on the Log Tab to search onlu User
   Selected       : boolean;       // Used as a semaphore on the Restore tab to toggle selection of tables to be restored
   Sema           : boolean;       // Used as a semaphore to toggel select/deselect of Log entries on the Log Tab
   ClientContact  : string;        //
   ClientEmail    : string;        //
   ClientName     : string;        //
   ClientUnique   : string;        //
   ErrMsg         : string;        // Last error message
   FilesDir       : string;        //
   LPMSUpgrade    : string;        // Path to the LPMS_Upgrade utility
   OSName         : string;        // Name of the OS we are running on
   OSShort        : string;        // Short name of the OS we are running on
//   Password       : string;        //
   Prefix         : string;        // Used by the Setup funtion as a working field to hold the DBPrefix
   RestoreTables  : string;        // Used by the Restore function t hold the list of tables contained in a backup file
   ServerName     : string;        // The HostName where the LPMS_Server is running
   ServerPort     : string;        // The Port on which the LPMS_Server is listening
   SQLFile        : string;        // Used by the Setup function to hold the name of the Table DDL defintions file
   ThisGUID       : string;        // Contains LPMS' existing GUID - used for silent installs
   ThisInstall    : string;        // FQDSN of the Silent Install utility
   UserName       : string;        //
   UserKey        : string;        // The LPMS Key stored in the Registry
   LogList        : TStringList;   // Used as a string list to get the results of the Dissassemble function and by the RedAllLogRecords function
   RespList       : TStringList;   // Holds the components of the response from the Server

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
   function  FindTable(ThisTable: string; idx1: integer) : integer;
   procedure GetList(FileName: string);
//   procedure DoNewEncoding();
   procedure DoNewStylePass();
   procedure DoMultiCompany();
   procedure MoveItems();
   function  DM_Connect_DB() : boolean;
   function  DM_Open_Connection() : boolean;
   function  DM_Put_DB(S1: string; RunType: integer) : boolean;
   function  DM_PutSignature() : boolean;
   function  DeleteItem(Key: integer) : boolean;
   function  SetDateTimeDef(ThisType: integer) : boolean;
   function  ReadAllRecs(UserStr,DescStr,S2: string) : boolean;
   function  ReadAllLogRecs(UserStr,DescStr,S2: string) : boolean;
   procedure ProcessResponse(Response: string);
   function  ReplaceQuote(S1: string; ThisType: integer) : string;
   function  Assemble(List: TStringList) : string;
   function  Disassemble(Str: string; ThisType: integer) : TStringList;

public   { Publlic Declartions}
   DoRestore      : boolean;       // Used as a semaphore by the Restore function
   Proceed        : boolean;       // False when user clicked on 'Cancel' on the MultiCpy screen otherwise True
   DBPrefix       : string;        // The Database Prefix stored in the Registry
   KeepRegString  : string;        // Holds the actual Registry location depending on the platform and the value of MultiCompany
   PassPhrase     : string;        // Used by Input Query
   RegPath        : string;        // Path to the local INI file - Not used on Winblows
   RestoreHost    : string;        // Used by the Restore function to indicate the Host to which the Resotre must be done
   RestorePass    : string;        // Used by the Restore function to hold the Password of the Restore Host
   RestoreUser    : string;        // Used by the Restore function to hold the UserID of the Restore Host//   Result         : string;        //
   SecretPhrase   : string;        // Used by Vignere
   ThisDBPrefix   : string;        // Contains the DBPrefix that was selected during start-up
   ThisPass       : string;        // Used by 'OLD_ENCODING' to hold the Pass phrase
   ThisRes        : string;        // Result from InputQuery

   function  InputQueryM(ThisCap, Question: string; DispType: integer) : string;

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
   TYPE_MASK          = 0;
   TYPE_UNMASK        = 1;

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

   REC_Unique_Values = record
      Unique           : array [1..6] of string;
   end;

//--- Temporary - Allow both old style encoding and new style encoding but
//--- only one type at a time

//{$DEFINE OLD_ENCODING}

var
   FLPMS_UtilityApp: TFLPMS_UtilityApp;

   This_Key_Values    : REC_Key_Values;
   This_Key_Priv      : REC_Key_Priv;
   This_Unique_Values : REC_Unique_Values;
   ImportList         : TStringList;
   TableList          : TStringList;

{$IFDEF DARWIN}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; stdcall; external 'libbsd_utilities.dylib';
   function  MaskField(InputField: string; MaskType: integer): string; stdcall; external 'libbsd_utilities.dylib';
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.dylib';
   function  GetUnique(var Get_Unique_Values: REC_Unique_Values): integer; stdcall; external 'libbsd_utilities.dylib';
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'libbsd_utilities.dylib';
{$ENDIF}
{$IFDEF WINDOWS}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; stdcall; external 'BSD_Utilities.dll';
   function  MaskField(InputField: string; MaskType: integer): string; stdcall; external 'BSD_Utilities.dll';
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'BSD_Utilities.dll';
   function  GetUnique(var Get_Unique_Values: REC_Unique_Values): integer; stdcall; external 'BSD_Utilities.dll';
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'BSD_Utilities.dll';
{$ENDIF}
{$IFDEF LINUX}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; stdcall; external 'libbsd_utilities.so';
   function  MaskField(InputField: string; MaskType: integer): string; stdcall; external 'libbsd_utilities.so';
   function  DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.so';
   function  GetUnique(var Get_Unique_Values: REC_Unique_Values): integer; stdcall; external 'libbsd_utilities.so';
   function  Vignere(ThisType: integer; Phrase: string; const Key: string) : string; stdcall; external 'libbsd_utilities.so';
{$ENDIF}

implementation

   uses LPMS_UtilityMultiCpy, LPMS_InputQuery, LPMS_UtilitySelDB;

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
   idx1, NumParms, NumUnique : integer;
   ParmsFound                : boolean = True;
   ThisPassPhrase            : string;
   Params, Args              : TStringList;
{$IFDEF WINDOWS}
   RegIni                    : TRegistryIniFile;
{$ELSE}
   RegIni                    : TINIFile;
{$ENDIF}

begin

   CanUpdate     := False;
   Proceed       := True;
   RestoreActive := False;
   Selected      := False;

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

//--- Check whether we are dealing with a Multi Company situation

   FLPMS_UtilityMultiCpy := TFLPMS_UtilityMultiCpy.Create(Application);

   FLPMS_UtilityApp.Hide();
   FLPMS_UtilityMultiCpy.ShowModal();
   FLPMS_UtilityApp.Show();

   FLPMS_UtilityMultiCpy.Destroy;

//--- If Proceed is False then the User clicked on 'Cancel' - Terminate

   if Proceed = False then begin

      Application.Terminate;
      Exit;

   end;

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

{$IFDEF WINDOWS}
   CallHWND := 0;
{$ENDIF}

   try

      Params  := TStringList.Create;
      Args    := TStringList.Create;

      for idx1 := 1 to ParamCount do
         Args.Add(ParamStr(idx1));

//--- Call and execute the cmdlOptions function in the BSD_Utilities DLL to
//--- determine whether parameters to do a Silent install were passed

      NumParms := cmdlOptions('G:M:P:H:', Args, Params);

      if NumParms > 0 then begin

         idx1      := 0;
         NumParms := NumParms * 2;

         while idx1 < Params.Count do begin

            if Params.Strings[idx1] = 'G' then              // GUID for LPMS installation upgrade
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

      Params.Free;
      Args.Free;

   end;

//--- Attempt a silent uninstall and upgrade of LPMS if both GUID and Setup
//--- location were passed as parameters

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


//--- Retrieve this PC's Mac Addresses (Up to 6)

   NumUnique := GetUnique(This_Unique_Values);

   if NumUnique = 0 then begin

      Application.MessageBox('Unable to find a valid MACAddress - LPMS Utility cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
      Application.Terminate;
      Exit;

   end else
      edtUnique.Text := This_Unique_Values.Unique[1];

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
   NUM_VAL         = 48;
   TAB_REGISTER    =  0;
   TAB_SETUP       =  1;
   TAB_MAINTENANCE =  2;
   TAB_CONVERT     =  3;
   TAB_UPGRADE     =  4;
   TAB_SQL         =  5;
   TAB_RESTORE     =  6;
   TAB_LOG         =  7;

var
   idx        : integer;
   ThisPrefix : string;
   ThisList   : TListItem;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

//--- Reset the information on the Setup tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_SETUP then
      LockS_State := True;

//--- Reset the information on the Convert tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_CONVERT then begin

      edtCurrVersion.Clear();

      LockC_State         := True;
      stProgress.Caption  := '';
      StaticText3.Caption := 'LPMS Utility: Enter the information below then click on the [Get] button to retrieve the current Data Base version';

   end;

//--- Reset the information on the Upgrade tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_UPGRADE then begin

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

   if pgTabs.ActivePageIndex <> TAB_RESTORE then begin

      edtBackupFile.Clear();
      edtTitle.Clear();
      edtDate.Clear();
      edtMode.Clear();
      edtVersion.Clear();
      edtTime.Clear();
      edtType.Clear();

      LockB_State            := True;
      edtBackupFile.Enabled  := False;
      rbFull.Checked         := False;
      rbPartial.Checked      := False;
      cbType.Checked         := False;
      stMsgB.Caption         := 'LPMS Utility: Enter or select the backup file to restore then select the required options before clicking on ''Restore''';

   end;

//--- Reset the information on the Log Display tab for safety reasons

   if pgTabs.ActivePageIndex <> TAB_LOG then begin

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
      stMsgL.Caption         := 'LPMS Utility: Provide a valid ''User'', ''Password'' and ''Host'' then click on ''Current Log'' or select a Log Archive then click on ''Load'' to display the contents';

   end;

//--- Set up the tab selected by the user

{$IFDEF OLD_ENCODING}
   ThisPrefix := jvCipher.DecodeString(SecretPhrase,Copy(DBPrefix,1,6));
{$ELSE}
   ThisPrefix := Vignere(CYPHER_DEC,Copy(DBPrefix,1,6),SecretPhrase);
{$ENDIF}

   if pgTabs.ActivePageIndex = TAB_REGISTER then begin

      edtKey.Text    := UserKey;
      edtPrefix.Text := ThisPrefix;

      btnRegister.Default := True;
      btnCancelR.Caption  := 'Cancel';
      btnCancelR.Default  := False;
      edtName.SetFocus;

   end else if pgTabs.ActivePageIndex = TAB_SETUP then begin

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

         btnProcessF.Default := True;
         btnCancelF.Caption  := 'Cancel';
         btnCancelF.Default  := False;

         edtHostName.SetFocus;

      end;

   end else if pgTabs.ActivePageIndex = TAB_MAINTENANCE then begin

      edtPrefixM.Text := ThisPrefix;

      edtKeyM.Text        := UserKey;
      btnProcessM.Enabled := False;

      edtPrefixMChange(Sender);

      btnProcessM.Default := True;
      btnCancelM.Caption  := 'Cancel';
      btnCancelM.Default  := False;

      edtPrefixM.SetFocus;

   end else if pgTabs.ActivePageIndex = TAB_CONVERT then begin

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
         btnProcessC.Default    := True;
         btnCancelC.Caption     := 'Cancel';
         btnCancelC.Default     := False;

         edtHostNameC.SetFocus;

      end;

   end else if pgTabs.ActivePageIndex = TAB_UPGRADE then begin

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

         edtHostNameD.SetFocus;

      end;

   end else if pgTabs.ActivePageIndex = TAB_RESTORE then begin

      rbFull.Checked      := True;
      cbType.Checked      := False;
      btnProcessB.Enabled := False;
      btnAllB.Enabled     := False;
      lvTables.Enabled    := False;
      Selected            := True;

      lvTables.Clear;

      if LockB_State = True then begin

         btnLockB.Visible      := True;
         btnUnlockB.Visible    := False;
         btnAllB.Enabled       := False;

         edtBackupFile.Enabled := False;
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

         edtBackupFile.SetFocus;

      end;

   end else if pgTabs.ActivePageIndex = TAB_LOG then begin

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

      edtUserL.SetFocus;

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
procedure TFLPMS_UtilityApp.imgRegisterClick(Sender: TObject);
begin

   saAbout.ShowAbout;

end;

(******************************************************************************)

{==============================================================================}
{--- Register Tab functions                                                 ---}
{==============================================================================}

//------------------------------------------------------------------------------
// User clicked on the Register button on the Register Page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnRegisterClick(Sender: TObject);
var
   Response, PlainReq, CodedReq : string;

begin

   if Trim(edtName.Text) = '' then begin

      Application.MessageBox('Name is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtName.SetFocus;
      Exit;

   end;

   if Trim(edtEmail.Text) = '' then begin

      Application.MessageBox('Email is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtEmail.SetFocus;
      Exit;

   end;

   if Trim(edtNum.Text) = '' then begin

      Application.MessageBox('Contact Num is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtNum.SetFocus;
      Exit;

   end;

   if Trim(edtCompany.Text) = '' then begin

      Application.MessageBox('Company is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtCompany.SetFocus;
      Exit;

   end;

//--- Connect to the Server and request registration

   tcpClient.Disconnect;
   tcpClient.Host := ServerName;
   tcpClient.Port := StrToInt(ServerPort);
   tcpClient.Connect;

//--- Identify ourselves to the Server and get the Server's response

   tcpClient.IOHandler.WriteLn('LPMS Server Request');
   Response := tcpClient.IOHandler.ReadLn;

   if Response = 'LPMS Server Ready' then begin

      PlainReq := '4|' + edtName.Text + '|' + edtEmail.Text + '|' + edtNum.Text + '|' + edtCompany.Text + '|' + edtUnique.Text + '|' + edtPrefix.Text + '|';
      CodedReq := Vignere(CYPHER_ENC,PlainReq,SecretPhrase);
      tcpClient.IOHandler.WriteLn(CodedReq);
      Response := tcpClient.IOHandler.ReadLn;
      ProcessResponse(Response);

   end else begin

      Application.MessageBox(PChar('Unexpected or invalid response from Server: "' + Response + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      tcpClient.Disconnect;
      Exit;

   end;

//--- Wrap it up

   tcpClient.Disconnect;

   btnRegister.Default := False;
   btnCancelR.Caption  := 'Close';
   btnCancelR.Default  := True;
   btnCancelR.SetFocus;

end;

//------------------------------------------------------------------------------
// A field on the Registration Page changed
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtNameChange(Sender: TObject);
var
   FldCount : integer = 0;

begin

   if edtName.Text    <> '' then Inc(FldCount);
   if edtEmail.Text   <> '' then Inc(FldCount);
   if edtNum.Text     <> '' then Inc(FldCount);
   if edtCompany.Text <> '' then Inc(FldCount);

   if FldCount = 4 then begin

//--- Online registration via LPMS_Utility is allowed for 'evl001' only

      if Trim(edtPrefix.Text) = 'evl001' then
         btnRegister.Enabled := True;

   end else
      btnRegister.Enabled := False;

end;

{==============================================================================}
{--- Setup Tab functions                                                    ---}
{==============================================================================}

//------------------------------------------------------------------------------
// User clicked on the Process button on the Setup Page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnProcessFClick(Sender: TObject);
var
   PartValid            : boolean;
   idx1                 : integer;
   PartOne, PartTwo, S1 : string;
   SQLList              : TStringList;
{$IFDEF WINDOWS}
   RegIni               : TRegistryIniFile;
{$ELSE}
   RegIni               : TINIFile;
{$ENDIF}

begin

   if Trim(edtHostName.Text) = '' then begin

      Application.MessageBox('Host Name is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtHostName.SetFocus;
      Exit;

   end;

   if Trim(edtUserName.Text) = '' then begin

      Application.MessageBox('User Name is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtUserName.SetFocus;
      Exit;

   end;

   if Trim(edtPassword.Text) = '' then begin

      Application.MessageBox('Password is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtPassword.SetFocus;
      Exit;

   end;

   if Trim(edtSQLFile.Text) = '' then begin

      Application.MessageBox('SQL File is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtSQLFile.SetFocus;
      Exit;

   end;

   if Trim(edtCpyName.Text) = '' then begin

      Application.MessageBox('Company Name is a required field - Please provide','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtCpyName.SetFocus;
      Exit;

   end;

//--- Make sure that Prefix contains exactly 6 characters with the first 3 alpa
//--- and the last three numeric

   if Length(edtPrefixF.Text) <> 6 then begin

      Application.MessageBox('Prefix is invalid. A valid Prefix is 6 characters in length and consists of 3 Alphabetic characters followed by 3 Numeric characters. Please provide a valid Prefix','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtPrefixF.SetFocus;
      Exit;

   end;

   PartOne   := Copy(edtPrefixF.Text,1,3);
   PartTwo   := Copy(edtPrefixF.Text,4,3);
   PartValid := True;

   for idx1 := 1 to 3 do begin

      if (PartOne[idx1] in ['A'..'Z','a'..'z']) = False then begin
         PartValid := False;
         break;
      end;

   end;

   for idx1 := 1 to 3 do begin

      if (PartTwo[idx1] in ['0'..'9']) = False then begin
         PartValid := False;
         break;
      end;

   end;

   if PartValid = False then begin
      Application.MessageBox('Prefix is invalid. A valid Prefix is 6 characters in length and consists of 3 Alphabetic characters followed by 3 Numeric characters. Please provide a valid Prefix','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtPrefixF.SetFocus;
      Exit;

   end;

//--- Warn the user that this action will utterly destroy any existing database

   if (Application.MessageBox(PChar('WARNING: This action will completely destroy any existing LPMS database for "' + edtPrefixF.Text + '" in "' + edtHostName.Text + '" and cannot be undone without a valid backup. You can:' + #10 + #10 + #10 + 'Click [OK] to proceed; or' + #10 + #10 + 'Click on [Cancel] to cancel this action'),'LPMS Utility',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL) then
      Exit;

//--- User wants to proceed

//   HostName := edtHostName.Text;
   Prefix   := edtPrefixF.Text;
   SQLFile  := edtSQLFile.Text;

//--- Build the DB connection string

   SQLCon.HostName     := edtHostName.Text;
   SQLCon.UserName     := edtUserName.Text;
   SQLCon.Password     := edtPassword.Text;
   SQLCon.DatabaseName := 'mysql';          // Temporary until we have a new database
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;

//--- Open a connection to the datastore named in HostName

   if DM_Open_Connection() = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Start by creating the Database

   S1 := 'DROP DATABASE IF EXISTS ' + Prefix + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Information message: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

   end;

   S1 := 'CREATE DATABASE ' + Prefix + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   SQLCon.DatabaseName := edtPrefixF.Text + '_LPMS';  // Now we have a database

//--- Define the default LPMS user for both local and remote access

   S1 := 'DROP USER "' + Prefix + '_LD"@"localhost"';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Information message: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

   end;

   S1 := 'CREATE USER "' + Prefix + '_LD"@"localhost" IDENTIFIED BY "LD01"';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   S1 := 'GRANT SELECT, INSERT, UPDATE, DELETE ON ' + Prefix + '_LPMS.* TO "' + Prefix + '_LD"@"localhost"';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   S1 := 'DROP USER "' + Prefix + '_LD"@"%"';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Information message: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

   end;

   S1 := 'CREATE USER "' + Prefix + '_LD"@"%" IDENTIFIED BY "LD01"';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   S1 := 'GRANT SELECT, INSERT, UPDATE, DELETE ON ' + Prefix + '_LPMS.* TO "' + Prefix + '_LD"@"%"';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Create the tables

   S1 := 'USE ' + Prefix + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   try

      SQLList := TStringList.Create;
      SQLList.LoadFromFile(SQLFile);

      for idx1 := 0 to SQLList.Count - 1 do begin

         if SQLList.Strings[idx1][1] = '#' then
            continue;

         if SQLList.Strings[idx1][1] = '1' then begin

            if (DM_Put_DB(SQLList.Strings[idx1].SubString(1,Length(SQLList.Strings[idx1]) - 1),TYPE_OTHER) = False) then
               Application.MessageBox(PChar('Information message: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

         end else begin

            if (DM_Put_DB(SQLList.Strings[idx1].SubString(1,Length(SQLList.Strings[idx1]) - 1),TYPE_SELECT) = False) then
               Application.MessageBox(PChar('Information message: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

         end;

      end;

   finally

      SQLList.Free;

   end;

//--- Write the LPMS Signature to the Database

   if DM_PutSignature() = False then
      Exit;

//--- Insert the values of HostName, DBPrefix and LastUser into the Registry

{$IFDEF OLD_ENCODING}
   DBPrefix := jvCipher.EncodeString(ThisPass,(edtPrefixF.Text + FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now)));
{$ELSE}
   DBPrefix := Vignere(CYPHER_ENC,edtPrefixF.Text,SecretPhrase) + MaskField(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now),TYPE_MASK);
{$ENDIF}


{$IFDEF WINDOWS}
   RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
   RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}

   RegIni.WriteString('Preferences','HostName',edtHostName.Text);
   RegIni.WriteString('Preferences','DBPrefix',DBPrefix);
   RegIni.WriteString('Preferences','LastUser','Administrator');

   RegIni.Destroy;

//--- Wrap it up

   Application.MessageBox('Set-up successfully completed...','LPMS Utility',(MB_OK + MB_ICONINFORMATION));

   btnProcessF.Default := False;
   btnCancelF.Caption  := 'Close';
   btnCancelF.Default  := True;
   btnCancelF.SetFocus;

end;

//------------------------------------------------------------------------------
// A Field on the Set-up Page changed
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtHostNameChange(Sender: TObject);
var
   FldCount : integer = 0;

begin

   if Trim(edtHostName.Text) <> '' then Inc(FldCount);
   if Trim(edtUserName.Text) <> '' then Inc(FldCount);
   if Trim(edtPassword.Text) <> '' then Inc(FldCount);
   if Trim(edtCpyName.Text)  <> '' then Inc(FldCount);

   if FldCount = 4 then
      btnProcessF.Enabled := True
   else
      btnProcessF.Enabled := False;

end;

//------------------------------------------------------------------------------
// User clicked on the embedded button in the SQL File field
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtSQLFileButtonClick(Sender: TObject);
begin

   edtSQLFile.Filter      := 'SQL Text Files (*.txt)|*.txt|SQL Files (*.sql)|*.sql|All Files (*.*)|*.*';
   edtSQLFile.DefaultExt  := '.txt';
   edtSQLFile.FilterIndex := 1;

end;

//------------------------------------------------------------------------------
// User Clicked on the Lock/Unlock at the bottom of the Set-up Page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnLockSClick(Sender: TObject);
var
   Passwd : string;

begin

   if LockS_State = True  then begin

      Passwd := InputQueryM('LPMS Utility','Pass phrase:',ord(TYPE_PASSWORD));

      if Passwd = PassPhrase then
         LockS_State := False;

   end else
      LockS_State := True;

   pgTabsChange(Sender);

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
   idx1             : integer;
   PartValid        : boolean;
   PartOne, PartTwo : string;
{$IFDEF WINDOWS}
   RegIni           : TRegistryIniFile;
{$ELSE}
   RegIni           : TINIFile;
{$ENDIF}

begin

   if Length(edtPrefixM.Text) <> 6 then begin

      Application.MessageBox('Prefix is invalid. A valid Prefix is 6 characters in length and consists of 3 Alphabetic characters followed by 3 Numeric characters. Please provide a valid Prefix','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtPrefixM.SetFocus;
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
      edtPrefixM.SetFocus;
      Exit;

   end;

   if Length(Trim(edtKeyM.Text)) <> 38 then begin

      Application.MessageBox('Key is a required field and must be exactly 38 characters long - Please provide a valid Key','LPMS Utility',(MB_OK + MB_ICONSTOP));
      edtKeyM.SetFocus;
      Exit;

   end;

{$IFDEF OLD_ENCODING}
   DBPrefix := jvCipher.EncodeString(ThisPass,(edtPrefixM.Text + FormatDateTime('yyyy/mm/dd',Now())));
{$ELSE}
   DBPrefix := Vignere(CYPHER_ENC,edtPrefixM.Text,SecretPhrase) + MaskField(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now),TYPE_MASK);
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
   btnCancelM.SetFocus;

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
{--- Convert Tab functions                                                  ---}
{==============================================================================}

//------------------------------------------------------------------------------
// User clicked on the Lock/Unlock button on the Convert page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnUnlockCClick(Sender: TObject);
var
   Passwd : string;

begin

   if LockC_State = True then begin

      Passwd := InputQueryM('LPMS Utility','Pass phrase:',ord(TYPE_PASSWORD));

      if Passwd = PassPhrase then
         LockC_State := False;

   end else begin

      LockC_State := True;
      edtHostName.Clear;
      edtUserName.Clear;
      edtPassword.Clear;
      edtCurrVersion.Clear;
      stProgress.Caption := '';
      StaticText3.Caption := 'LPMS Utility: Enter the information below then click on the [Get] button to retrieve the current Data Base version';

   end;

   pgTabsChange(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the Process button on the Convert page
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnProcessCClick(Sender: TObject);
var
   idx1, Len        : integer;
   S1, S2, CurrDate, ThisStr : string;
   ThisList         : TStringList;

begin

//--- Build the DB connection string

   SQLCon.HostName     := edtHostNameC.Text;
   SQLCon.UserName     := edtUserNameC.Text;
   SQLCon.Password     := edtPasswordC.Text;
   SQLCon.DatabaseName := edtPrefixC.Text + '_LPMS';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;

   Prefix   := edtPrefixC.Text;

   if DM_Open_Connection = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Select the Database to use

   S1 := 'USE ' + Prefix + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Check if MultiCompany Support is selected and if so whether a DBPrefix was
//--- specified

   if cbMultiCpy.Checked = True then begin

      if Trim(edtNewPrefixC.Text) = '' then begin

         Application.MessageBox('"New Prefix:" must be specified if "Multi Company Support" is selected.','LPMS Utility',(MB_OK + MB_ICONSTOP));
         edtNewPrefixC.SetFocus;
         Exit;

      end;

   end;

//--- Convert from 2.2.1 to 3.0.1

   if edtCurrVersion.Text = '2.2.1' then begin


//--- Update the 'control' table

      stProgress.Caption := 'Converting "control" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE control ADD COLUMN Control_';

      S2 := S1 + 'DoBilling Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'UpBilling Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Switch1 Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Switch2 Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Switch3 Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Switch4 Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Switch5 Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE control SET Control_UpBilling = 1 WHERE Control_UserType = "Administrator"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE control SET Control_DoBilling = 1 WHERE Control_FeeEarner = 1';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'DELETE FROM control WHERE Control_UserID = "Backup Administrator"';
      S2 := 'INSERT INTO control (Control_UserID, Control_Password, Control_Name, Control_Email, Control_UserType, Control_FeeEarner, Control_DoBilling, Control_UpBilling, Control_Switch1, Control_Switch2, Control_Switch3, Control_Switch4, Control_Switch5, Control_Unique, Create_By, Create_Date, Create_Time) Values("Backup Administrator", "«ßÞÉµÓ‘Ÿ", "Default Backup Administrator", "lpms@bluecrane.cc", "Administrator", 0, 0, 0, 1, 0, 0, 0, 0, 0, "LPMS_FirstRun", "' +
           FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now()) + '", "'  +
           FormatDateTime('HH:mm:ss',Now) + '")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'tracking' table to enable Collections

      stProgress.Caption := 'Converting "tracking" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE tracking ADD COLUMN Tracking_';

      S2 := S1 + 'IntDate VarChar (32)';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Capital Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Int Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Comm Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Contingency Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Fees Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Prescribed Integer (11) DEFAULT 1';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      CurrDate := FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now());
      S2 := 'UPDATE tracking SET Tracking_IntDate = "' + CurrDate +
           '", Tracking_Comm = 10.00, Tracking_Int = 15.50, Tracking_Contingency = 25.00, Tracking_Prescribed = 1';

      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE tracking SET Tracking_FileType = 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'notes' table

      stProgress.Caption := 'Converting "notes" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE notes ADD COLUMN Notes_User VarChar (255) DEFAULT "System"';
      S2 := 'UPDATE notes SET Notes_User = "System"';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'customers' table

      stProgress.Caption := 'Converting "customers" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE customers ADD COLUMN Cust_FreeText VarChar (1024)';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'doccntl' table

      stProgress.Caption := 'Converting "doccntl" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE doccntl ADD COLUMN DocCntl_Facility VarChar(512)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE doccntl ADD COLUMN DocCntl_Container VarChar(255)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE doccntl ADD COLUMN DocCntl_Box VarChar(255)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE doccntl ADD COLUMN DocCntl_Status Integer(11)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE doccntl SET DocCntl_Status = 0 WHERE (DocCntl_DateIn = "Checked Out" OR DocCntl_DateIn = "Unset")';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE doccntl SET DocCntl_Status = 1 WHERE (DocCntl_DateOut <> "Service" AND DocCntl_DateIn <> "Checked Out" AND DocCntl_DateIn <> "Unset")';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE doccntl SET DocCntl_Status = 2 WHERE DocCntl_DateOut = "Service"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert the 'collect' table

      stProgress.Caption := 'Creating "collect" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE collect (Collect_Key int(11) NOT NULL AUTO_INCREMENT, Collect_Type int(11) DEFAULT NULL, Collect_Class int(11) DEFAULT NULL, Collect_Date varchar(32) DEFAULT NULL, Collect_Item varchar(128) DEFAULT NULL, Collect_AccountType int(11) DEFAULT NULL, Collect_Description varchar(1024) DEFAULT NULL, Collect_UnitType int(11) DEFAULT NULL, Collect_Units double DEFAULT NULL, Collect_Calculation int(11) DEFAULT NULL, Collect_Label int(11) DEFAULT NULL, Collect_Inc1 int(11) DEFAULT NULL, Collect_Inc2 int(11) DEFAULT NULL, Collect_Minimum double DEFAULT NULL, Collect_Maximum double DEFAULT NULL, Collect_Fixed double DEFAULT NULL, Collect_Rate double DEFAULT NULL, Collect_DrCr int(11) DEFAULT NULL, Collect_Amount double DEFAULT NULL, Collect_Owner varchar(10) DEFAULT NULL, Collect_Related varchar(10) DEFAULT NULL, Collect_FeeEarner int(11) DEFAULT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", Collect_TimeStamp varchar(128) DEFAULT NULL, PRIMARY KEY (Collect_Key)) ENGINE=MyISAM ROW_FORMAT=dynamic DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert the 'invoices' table

      stProgress.Caption := 'Creating "invoices" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE invoices (Inv_Key int(11) NOT NULL AUTO_INCREMENT, Inv_Invoice varchar(32) DEFAULT NULL, Inv_File varchar(10) DEFAULT NULL, Inv_Amount varchar(32) DEFAULT "0.00", Inv_Description varchar(255) DEFAULT NULL, Inv_HostName varchar(512) DEFAULT NULL, Inv_SDate varchar(10) DEFAULT NULL, Inv_EDate varchar(10) DEFAULT NULL, Inv_AcctType int(11) DEFAULT NULL, Inv_ShowRelated int(11) DEFAULT NULL, Inv_Fees VarChar(32) DEFAULT "0.00", Inv_Disburse VarChar(32) DEFAULT "0.00", Inv_Expenses VarChar(32) DEFAULT "0.00", Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", Inv_TimeStamp varchar(128) DEFAULT NULL, PRIMARY KEY (Inv_Key)) ENGINE=MyISAM ROW_FORMAT=dynamic DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci;';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'lpms' table

      stProgress.Caption := 'Converting "lpms" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE lpms ADD COLUMN ';

      S2 := S1 + 'InvoiceIdx Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'InvoicePref VarChar(8) DEFAULT "Inv"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Blocked Integer(11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Reason VarChar(1024)';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billing' table to change 'Trust Transfer (Fees)' to 'Trust Transfer (Business)'

      stProgress.Caption := 'Updating "billing" Table';
      Application.ProcessMessages;

      S1 := 'UPDATE billing SET B_Item = "Trust Transfer (Business)" WHERE B_Item = "Trust Transfer (Fees)"';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billingitems' table to change 'Trust Transfer (Fees)' to 'Trust Transfer (Business)'

      stProgress.Caption := 'Updating "billingitems" Table';
      Application.ProcessMessages;

      S1 := 'UPDATE billingitems SET Bi_Item = "Trust Transfer (Business)" WHERE Bi_Item = "Trust Transfer (Fees)"';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert the 'payments' table

      stProgress.Caption := 'Creating "payments" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE payments (Pay_Key int(11) NOT NULL AUTO_INCREMENT, Pay_Invoice varchar(32) DEFAULT NULL, Pay_Date varchar(10) DEFAULT "Not Paid", Pay_Amount varchar(32) DEFAULT "0.00", Pay_File varchar(10) DEFAULT NULL, Pay_Note varchar(1024) DEFAULT NULL, Pay_Billing varchar(128) DEFAULT "No Billing", Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", Pay_TimeStamp varchar(128) DEFAULT NULL, PRIMARY KEY (Pay_Key)) ENGINE=MyISAM ROW_FORMAT=dynamic DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the database version

      stProgress.Caption := 'Updating database version';
      Application.ProcessMessages;

      S2 := 'UPDATE lpms SET Version = "3.0.1"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      SQLQry1.Close();
      SQLCon.Close();

      Application.MessageBox(PChar('Database "' + edtHostNameC.Text + '[' + edtPrefixC.Text + ']" updated from version "' + edtCurrVersion.Text + '" to version "' + edtNewVersion.Text + '"'),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

      stProgress.Caption := '';
      Application.ProcessMessages;

   end;

//--- Convert from any version less than 3.2.0

   if edtCurrVersion.Text < '3.2.0' then begin


//--- Update the 'invoices' table

      stProgress.Caption := 'Converting "invoices" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE invoices ADD COLUMN Inv_';

      S2 := S1 + 'Fees VarChar(32) DEFAULT "0.00"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Disburse VarChar(32) DEFAULT "0.00"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'Expenses VarChar(32) DEFAULT "0.00"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert the 'payments' table

      stProgress.Caption := 'Creating "payments" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE payments (Pay_Key int(11) NOT NULL AUTO_INCREMENT, Pay_Invoice varchar(32) DEFAULT NULL, Pay_Date varchar(10) DEFAULT "Not Paid", Pay_Amount varchar(32) DEFAULT "0.00", Pay_File varchar(10) DEFAULT NULL, Pay_Note varchar(1024) DEFAULT NULL, Pay_Billing varchar(128) DEFAULT "No Billing", Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", Pay_TimeStamp varchar(128) DEFAULT NULL, PRIMARY KEY (Pay_Key)) ENGINE=MyISAM ROW_FORMAT=dynamic DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'payments' table to add Pay_Billing

      stProgress.Caption := 'Updating "payments" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE payments ADD COLUMN Pay_';

      S2 := S1 + 'Billing VarChar(128) DEFAULT "No Billing"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billing' table to change 'Trust Transfer (Fees)' to 'Trust Transfer (Business)'

      stProgress.Caption := 'Updating "billing" Table';
      Application.ProcessMessages;

      S1 := 'UPDATE billing SET B_Item = "Trust Transfer (Business)" WHERE B_Item = "Trust Transfer (Fees)"';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billing' table to add the Expense column

      S1 := 'ALTER TABLE billing ADD COLUMN B_Expense Double DEFAULT 0';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billing' table to add 'Reserved Deposit' functionality

      S1 := 'ALTER TABLE billing ADD COLUMN B_';

      S2 := S1 + 'ReserveDep Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'ReserveAmt Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'ReserveReason VarChar(1024) DEFAULT "No Reserved Deposit"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'collect' table to add the Expense column

      stProgress.Caption := 'Updating "collect" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE collect ADD COLUMN Collect_Expense Double DEFAULT 0';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'collect' table to add 'Reserved Deposit' functionality

      S1 := 'ALTER TABLE collect ADD COLUMN Collect_';

      S2 := S1 + 'ReserveDep Integer (11) DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'ReserveAmt Double DEFAULT 0';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := S1 + 'ReserveReason VarChar(1024) DEFAULT "No Reserved Deposit"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billingitems' table to change 'Trust Transfer (Fees)' to 'Trust Transfer (Business)'

      stProgress.Caption := 'Updating "billingitems" Table';
      Application.ProcessMessages;

      S1 := 'UPDATE billingitems SET Bi_Item = "Trust Transfer (Business)" WHERE Bi_Item = "Trust Transfer (Fees)"';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billingitems' table to add the Expense column

      S1 := 'ALTER TABLE billingitems ADD COLUMN Bi_Expense Double DEFAULT 0';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'tracking' table to add the Prescribed interest column

      stProgress.Caption := 'Updating "tracking" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE tracking ADD COLUMN Tracking_';

      S2 := S1 + 'Prescribed Integer (11) DEFAULT 1';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S2 := 'UPDATE tracking SET Tracking_Prescribed = 1';

      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'customers' table to add the FreeText column if it does not exist

      stProgress.Caption := 'Updating "customers" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE customers ADD COLUMN Cust_FreeText VarChar (1024) DEFAULT NULL';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

{
//--- Convert Passwords to New Style Passwords if the flag was checked

      DoNewStylePass;

//--- Convert to Multi Company support if cbMuliCpy is checked

      DoMultiCompany;

//--- Update the database version

      stProgress.Caption := 'Updating database version';
      Application.ProcessMessages;

      S2 := 'UPDATE lpms SET Version = "3.1.0"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      Application.MessageBox(PChar('Database '' + edtHostNameC.Text + '[' + edtPrefixC.Text + ']' updated from version '' + edtCurrVersion.Text + '' to version '' + edtNewVersion.Text + '''),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

      stProgress.Caption := '';
      Application.ProcessMessages;

   end;

//--- Add additional items that were added since 3.1.0 originally went live

   if edtCurrVersion.Text = '3.1.0' then begin


//--- Convert Passwords to New Style Passwords if the flag was checked

      DoNewStylePass;

//--- Convert to Multi Company support if cbMuliCpy is checked

      DoMultiCompany;
}

//--- Add the Instant Message table

      stProgress.Caption := 'Creating "im" Table';
      Application.ProcessMessages;

//--- Insert the 'im' table

      S1 := 'CREATE TABLE im (Im_Key int(11) NOT NULL AUTO_INCREMENT, Im_Date varchar(10) DEFAULT NULL, Im_Time varchar(12) DEFAULT NULL, Im_From varchar(64) DEFAULT NULL, Im_To varchar(64) DEFAULT NULL, Im_Message varchar(512) DEFAULT NULL, Im_Status int(11) DEFAULT NULL, PRIMARY KEY (Im_Key)) ENGINE=MyISAM ROW_FORMAT=dynamic DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Add a default message for each user

      try

         ThisList := TStringList.Create;

         S1 := 'SELECT Control_UserID FROM control ORDER BY Control_UserID';
         if DM_Put_DB(S1,TYPE_SELECT) = False then begin

            if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
               Exit;

         end;

         SQLQry1.First;

         for idx1 := 1 to SQLQry1.RecordCount do begin

            ThisList.Add(SQLQry1.FieldByName('Control_UserID').AsString);
            SQLQry1.Next;

         end;

         for idx1 := 0 to ThisList.Count - 1 do begin

            S2 := 'Hi ' + ThisList.Strings[idx1] + ', Welcome to LPMS Internal Messaging. To send a message is easy - Select a User from the list on the Left, type your message and then click on [Send]';

            S1 := 'INSERT INTO im (Im_Date, Im_Time, Im_From, Im_To, Im_Message, Im_Status) Values("' +
                  FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) +
                  '", "'  + FormatDateTime('HH:mm:ss.zzz',Now) +
                  '", "LPMS_Utility", "' + ThisList.Strings[idx1] + '", "' +
                  ReplaceQuote(S2,TYPE_WRITE) + '", ' + IntToStr(IM_UNREAD) + ')';

            if DM_Put_DB(S1,TYPE_OTHER) = False then begin

               if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                  Exit;

            end;

            S2 := 'You can change the interval for automatically checking new messages and the colours in which messages are dislayed by clicking on the [IM Prefs] button in the <Functions> section on the <Preferences> Screen. Unread messages are displayed in Bold';

            S1 := 'INSERT INTO im (Im_Date, Im_Time, Im_From, Im_To, Im_Message, Im_Status ) Values("' +
                  FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now()) +
                  '", "'  + FormatDateTime('HH:mm:ss.zzz',Now) +
                  '", "LPMS_Utility", "' + ThisList.Strings[idx1] + '", "' +
                  ReplaceQuote(S2,TYPE_WRITE) + '", ' + IntToStr(IM_UNREAD) + ')';

            if DM_Put_DB(S1,TYPE_OTHER) = False then begin

               if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                  Exit;

            end;

         end;

      finally

         ThisList.Free;

      end;

//--- Insert the 'tbinterest' table

      stProgress.Caption := 'Creating "tbinterest" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE tbinterest (TbInt_Key int(11) NOT NULL AUTO_INCREMENT, TbInt_SetName varchar(1024) DEFAULT "Not Defined", TbInt_EffectiveDate varchar(10) DEFAULT NULL, TbInt_Rate double DEFAULT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", PRIMARY KEY (TbInt_Key)) ENGINE=MyISAM DEFAULT CHARSET=latin1';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert default interest rates sets

      S1 := 'INSERT INTO tbinterest (TbInt_SetName, TbInt_EffectiveDate, TbInt_Rate, Create_Date, Create_Time, Create_By) Values("Before 01 August 2014", "1980/01/01", 15.00, "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) +
            '", "' + FormatDateTime('HH:mm:ss.zzz',Now) + '", "LPMS_Utility")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'INSERT INTO tbinterest (TbInt_SetName, TbInt_EffectiveDate, TbInt_Rate, Create_Date, Create_Time, Create_By) Values("Efective 01 August 2014", "2014/08/01", 9.00, "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) +
            '", "' + FormatDateTime('HH:mm:ss.zzz',Now) + '", "LPMS_Utility")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'INSERT INTO tbinterest (TbInt_SetName, TbInt_EffectiveDate, TbInt_Rate, Create_Date, Create_Time, Create_By) Values("Effective 01 March 2016", "2016/03/01", 10.25, "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) +
            '", "' + FormatDateTime('HH:mm:ss.zzz',Now) + '", "LPMS_Utility")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert the 'tbbilling' table

      stProgress.Caption := 'Creating "tbbilling" Table';
      Application.ProcessMessages;

      FilesDir := ExtractFilePath(Application.ExeName);
      FilesDir := AnsiReplaceStr(FilesDir,'\Bin\','\Files\');
      FilesDir := AnsiReplaceStr(FilesDir,'\Release\','\Files\');
      FilesDir := AnsiReplaceStr(FilesDir,'\Debug\','\Files\');

      S1 := 'CREATE TABLE tbbilling (TbBilling_Key int(11) NOT NULL AUTO_INCREMENT, TbBilling_SetName varchar(1024) DEFAULT NULL, TbBilling_EffectiveDate varchar(10) DEFAULT NULL, TbBilling_Definition varchar(1024) DEFAULT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", PRIMARY KEY (TbBilling_Key)) ENGINE=MyISAM DEFAULT CHARSET=latin1';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert default billing definition sets

      S1 := 'INSERT INTO tbbilling (TbBilling_SetName, TbBilling_EffectiveDate, TbBilling_Definition, Create_Date, Create_Time, Create_By) Values("Before 01 August 2014", "1980/01/01", "' + (ReplaceQuote(FilesDir,TYPE_WRITE) + 'Before 01 August 2014.lbd') + '", "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) + '", "' +
            FormatDateTime('HH:mm:ss.zzz',Now) + '", "LPMS_Utility")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'INSERT INTO tbbilling (TbBilling_SetName, TbBilling_EffectiveDate, TbBilling_Definition, Create_Date, Create_Time, Create_By) Values("Efective 01 August 2014", "2014/08/01", "' + (ReplaceQuote(FilesDir,TYPE_WRITE) + 'Effective 01 August 2014') + '", "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) + '", "' +
            FormatDateTime('HH:mm:ss.zzz',Now) + '", "LPMS_Utility")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'INSERT INTO tbbilling (TbBilling_SetName, TbBilling_EffectiveDate, TbBilling_Definition, Create_Date, Create_Time, Create_By) Values("Effective 01 March 2016", "2016/03/01", "' + (ReplaceQuote(FilesDir,TYPE_WRITE) + 'Effective 01 March 2016') + '", "' +
            FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) + '", "' +
            FormatDateTime('HH:mm:ss.zzz',Now) + '", "LPMS_Utility")';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Insert the 'symvars' table

      stProgress.Caption := 'Creating "symvars" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE symvars (Sym_Key int(11) unsigned zerofill NOT NULL AUTO_INCREMENT, Sym_Owner varchar(64) NOT NULL, Sym_Variable varchar(128) NOT NULL, Sym_Value varchar(512) NOT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", PRIMARY KEY (Sym_Key)) ENGINE=MyISAM AUTO_INCREMENT=0 DEFAULT CHARSET=latin1';

      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Add 'Blocked' field to Users table

      stProgress.Caption := 'Updating "control" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE control ADD COLUMN Control_Blocked Integer (11) DEFAULT 0';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the database version

      stProgress.Caption := 'Updating database version';
      Application.ProcessMessages;

      S2 := 'UPDATE lpms SET Version = "3.2.0"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      Application.MessageBox(PChar('Database "' + edtHostNameC.Text + '[' + edtPrefixC.Text + ']" updated from version "' + edtCurrVersion.Text + '" to version 3.2.0"'),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

      stProgress.Caption := '';
      Application.ProcessMessages;

   end;

//--- Convert to version 3.2.1

   if edtCurrVersion.Text < '3.2.1' then begin


//===
//=== Todo Change the way the File Path is stored (Store only the File name)
//===

//--- Update the 'users' table to add Logging

      stProgress.Caption := 'Converting "control" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE control ADD COLUMN Control_Logging Integer (11) DEFAULT 3';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE control ADD COLUMN Control_Rate double DEFAULT 0.00';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'users' table to add 'Do Quote' for every user

      S1 := 'UPDATE control SET Control_Switch4 = 1';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'invoices' table

      stProgress.Caption := 'Converting "invoices" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE invoices ADD COLUMN Inv_Related varchar (10)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Select the unique File names in the Invoices table and add to a list

      S1 := 'SELECT DISTINCT Inv_File FROM invoices ORDER BY Inv_File';
      if DM_Put_DB(S1,TYPE_SELECT) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      try

         ThisList := TStringList.Create;
         SQLQry1.First;

         for idx1 := 1 to SQLQry1.RecordCount do begin

            ThisList.Add(SQLQry1.FieldByName('Inv_File').AsString);
            SQLQry1.Next;

         end;

//--- Get the related file information for each unique File in the Invoice table
//--- and add this to the list - each entry consist of xxxxxxyyyyyy where xxxxxx
//--- is the File name and yyyyyy is the Related file

         for idx1 := 0 to ThisList.Count - 1 do begin

            S1 := 'SELECT Tracking_Related FROM tracking WHERE Tracking_Name = "' + ThisList.Strings[idx1] + '"';

            if DM_Put_DB(S1,TYPE_SELECT) = False then begin

               if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                  Exit;

            end;

            ThisList.Strings[idx1] := ThisList.Strings[idx1] + SQLQry1.FieldByName('Tracking_Related').AsString;

         end;

//--- Update the invoices table with the related file names

         if ThisList.Count <= 1 then
            Len := 1
         else
            Len := ThisList.Strings[0].Length div 2;

         for idx1 := 0 to ThisList.Count - 1 do begin

            S1 := 'UPDATE invoices SET Inv_Related = "' +
                 ThisList.Strings[idx1].SubString(Len + 1, 99) +
                 '" WHERE Inv_File = "' +
                 ThisList.Strings[idx1].SubString(1,Len) + '"';

            if DM_Put_DB(S1,TYPE_OTHER) = False then begin

               if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                  Exit;

            end;

         end;

      finally

         ThisList.Free;

      end;

//--- Add the Log table

      stProgress.Caption := 'Creating "log" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE log (Log_Key int(11) NOT NULL AUTO_INCREMENT, Log_Date varchar(20) NOT NULL, Log_Time varchar(20) NOT NULL, Log_User  varchar(255) NOT NULL, Log_Activity varchar(4096) NOT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, TimeStamp varchar(128) NOT NULL, PRIMARY KEY (Log_Key)) ENGINE=MyISAM AUTO_INCREMENT=0 DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Add the Quotes table

      stProgress.Caption := 'Creating "quotes" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE quotes (Q_Key int(11) NOT NULL AUTO_INCREMENT, Q_Quote varchar(32) DEFAULT NULL, Q_Type int(11) DEFAULT NULL, Q_Class int(11) DEFAULT NULL, Q_Date varchar(32) DEFAULT NULL, Q_Item varchar(128) DEFAULT NULL, Q_AccountType int(11) DEFAULT NULL, Q_Description varchar(1024) DEFAULT NULL, Q_UnitType int(11) DEFAULT NULL, Q_Units double DEFAULT NULL, Q_Calculation int(11) DEFAULT NULL, Q_Label int(11) DEFAULT NULL, Q_Inc1 int(11) DEFAULT NULL, Q_Inc2 int(11) DEFAULT NULL, Q_Minimum double DEFAULT NULL, Q_Maximum double DEFAULT NULL, Q_Fixed double DEFAULT NULL, Q_Rate double DEFAULT NULL, Q_Expense double DEFAULT "0", Q_DrCr int(11) DEFAULT NULL, Q_Amount double DEFAULT NULL, Q_Owner varchar(10) DEFAULT NULL, Q_Related varchar(10) DEFAULT NULL, Q_FeeEarner int(11) DEFAULT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", Q_TimeStamp varchar(128) DEFAULT NULL, PRIMARY KEY (Q_Key)) ENGINE=MyISAM AUTO_INCREMENT=0 DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Add the Accept table

      stProgress.Caption := 'Creating "accept" Table';
      Application.ProcessMessages;

      S1 := 'CREATE TABLE accept (Accept_Key int(11) NOT NULL AUTO_INCREMENT, Accept_File varchar(32) DEFAULT NULL, Accept_Quote varchar(32) DEFAULT NULL, Accept_Accepted int(11) DEFAULT NULL, Accept_Date varchar(32) DEFAULT NULL, Accept_HostName varchar(255) DEFAULT NULL, Accept_Description varchar(255) DEFAULT NULL, Create_Date varchar(32) DEFAULT NULL, Create_Time varchar(32) DEFAULT NULL, Create_By varchar(64) DEFAULT NULL, Modify_Date varchar(32) DEFAULT "Not Modified", Modify_Time varchar(32) DEFAULT "Not Modified", Modify_By varchar(64) DEFAULT "Not Modified", Accept_TimeStamp varchar(128) DEFAULT NULL, PRIMARY KEY (Accept_Key)) ENGINE=MyISAM AUTO_INCREMENT=0 DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the notes table then distinguish between billing related and file
//--- notes

      stProgress.Caption := 'Converting "notes" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE notes ADD COLUMN Notes_Type int(11) NOT NULL';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- First we set all Notes Types to File Notes

      S1 := 'UPDATE notes SET Notes_Type = 1';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Then we try and identify Billing Notes and set Notes Type for these
//--- records to Billing Notes

      S1 := 'UPDATE notes SET Notes_Type = 2 WHERE ((Notes_Note LIKE "Inserted new payment record on %") OR (Notes_Note LIKE "Inserted new billing record on %") OR (Notes_Note LIKE "Updated existing payment record on %") OR (Notes_Note LIKE "Updated existing billing record on %") OR (Notes_Note LIKE "Deleted existing billing record on %") OR (Notes_Note LIKE "Deleted existing payment record on %"))';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Split 'Trust Transfer (Business)' to distinguish between 'Fees/Disb/Exp'
//--- and 'Other' transfers

      stProgress.Caption := 'Updating "billingitems" Table';
      Application.ProcessMessages;

      S1 := 'UPDATE billingitems SET Bi_Item = "Trust Transfer (Business - Fees/Disb/Exp)" WHERE Bi_Item = "Trust Transfer (Business)"';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- First check whether the 'Trust Transfer (Business - Other)' class already
//--- exist to avoid duplicates

      S1 := 'SELECT Bi_Key FROM billingitems WHERE Bi_Item = "Trust Transfer (Business - Other)"';
      if DM_Put_DB(S1,TYPE_SELECT) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      if SQLQry1.RecordCount = 0 then begin

         S1 := 'INSERT INTO billingitems (Bi_Type, Bi_Class, Bi_User, Bi_Item, Bi_Description, Bi_UnitType, Bi_Units, Bi_Calculation, Bi_Label, Bi_Inc1, Bi_Inc2, Bi_Minimum, Bi_Maximum, Bi_Fixed, Bi_Rate, Bi_DrCr, Create_Date, Create_Time, Create_By, Bi_TimeStamp, Bi_Expense) Values(3, 24, "", "Trust Transfer (Business - Other)", "Transfer from Trust to Business for [Other][ on instruction of Client]", 6, "1", 8, 0, 0, 0, "0", "0", "0", "0", 1, "' +
                FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) +
                '", "' + FormatDateTime('HH:mm:ss',Now) +
                '", "LPMS_Utility", "' +
                FormatDateTime('yyyy/MM/ddHH:mm:ss:zzz',Now) + '+LPMS_Utility' +
                '", "0")';

         if DM_Put_DB(S1,TYPE_OTHER) = False then begin

            if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
               Exit;

         end;

      end;

      S1 := 'UPDATE billing SET B_Item = "Trust Transfer (Business - Fees/Disb/Exp)" WHERE B_Item = "Trust Transfer (Business)"';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'billing' table

      stProgress.Caption := 'Converting "billing" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE billing ADD COLUMN B_ToFile varchar (32)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE billing ADD COLUMN B_Unique varchar (128)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE billing ADD COLUMN B_InvestAcct varchar (32)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'UPDATE billing SET B_ReserveReason = "" WHERE ((B_Class <> 7) AND (B_Class <> 11))';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'collect' table

      stProgress.Caption := 'Converting "collect" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE collect ADD COLUMN Collect_ToFile varchar (32)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE collect ADD COLUMN Collect_Unique varchar (128)';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'prefix' table to add File Type and Folder Colour

      stProgress.Caption := 'Converting "prefix" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE prefix ADD COLUMN Prefix_FileType varchar (32) DEFAULT "Other"';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE prefix ADD COLUMN Prefix_FolderCol int (11) DEFAULT 0';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the 'lpms' table

      stProgress.Caption := 'Converting "lpms" Table';
      Application.ProcessMessages;

      S1 := 'ALTER TABLE lpms ADD COLUMN QuoteIdx double DEFAULT "0"';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      S1 := 'ALTER TABLE lpms ADD COLUMN QuotePref varchar (8) DEFAULT "Q"';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Correct the spelling of 'practise' in lpms

      S1 := 'UPDATE lpms SET Signature = "Legal Practice Management System - LPMS"';
      if DM_Put_DB(S1,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Update the database version

      stProgress.Caption := 'Updating database version';
      Application.ProcessMessages;

      S2 := 'UPDATE lpms SET Version = "3.2.1"';
      if DM_Put_DB(S2,TYPE_OTHER) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click [Yes] to continue; or' + #10 + #10 + 'Click [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      Application.MessageBox(PChar('Database "' + edtHostNameC.Text + '[' + edtPrefixC.Text + ']" updated from version "' + edtCurrVersion.Text + '" to version "' + edtNewVersion.Text + '"'),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));

      stProgress.Caption := '';
      Application.ProcessMessages;

   end;

//--- Convert Password Encoding to New Encoding if the flag was checked

//#ifdef NEW_ENCODING
//   DoNewEncoding;
//#endif

//--- Convert Passwords to New Style Passwords if the flag was checked

   DoNewStylePass;

//--- Convert to Multi Company support if cbMuliCpy is checked

   DoMultiCompany;

   SQLQry1.Close;
   SQLCon.Close;

   btnProcessC.Enabled := False;
   edtCurrVersion.Text := edtNewVersion.Text;

end;

//------------------------------------------------------------------------------
// A Field on the Convert page changed
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtHostNameCChange(Sender: TObject);
var
   FldCount : integer = 0;

begin

//--- Check if the Get button can be enabled

   if Trim(edtHostNameC.Text) <> '' then Inc(FldCount);
   if Trim(edtUserNameC.Text) <> '' then Inc(FldCount);
   if Trim(edtPasswordC.Text) <> '' then Inc(FldCount);

   if FldCount = 3 then
      btnGet.Enabled := True
   else
      btnGet.Enabled := False;

//--- Check if the Process button can be enabled

   FldCount := 0;

   if Trim(edtHostNameC.Text) <> '' then Inc(FldCount);
   if Trim(edtUserNameC.Text) <> '' then Inc(FldCount);
   if Trim(edtPasswordC.Text) <> '' then Inc(FldCount);
   if Trim(edtCurrVersion.Text) <> '' then Inc(FldCount);

   if FldCount = 4 then
      btnProcessC.Enabled := True
   else
      btnProcessC.Enabled := False;

end;

//------------------------------------------------------------------------------
// User clicked on the button to get the current database version
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnGetClick(Sender: TObject);
var
   S1 : string;

begin

//--- Build the DB connection string

   SQLCon.HostName     := edtHostNameC.Text;
   SQLCon.UserName     := edtUserNameC.Text;
   SQLCon.Password     := edtPasswordC.Text;
   SQLCon.DatabaseName := edtPrefixC.Text + '_LPMS';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;

   Prefix   := edtPrefixC.Text;

   if DM_Open_Connection = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Select the Database to use

   S1 := 'USE ' + Prefix + '_LPMS';

   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Get the current version from the database

   S1 := 'SELECT Version FROM lpms';

   if DM_Put_DB(S1,TYPE_SELECT) = False then begin

      Application.MessageBox(PChar('Error message: ' + ErrMsg),'LPMS Utility',(MB_OK + MB_ICONWARNING));
      Exit;

   end else begin

      edtCurrVersion.Text := SQLQry1.FieldByName('Version').AsString;
      SQLQry1.Close;
      SQLCon.Close;

   end;

   edtHostNameCChange(Sender);
   StaticText3.Caption := 'LPMS Utility: Click on the [Process] button to convert the Database from version "' + edtCurrVersion.Text + '" to version "' + edtNewVersion.Text + '"';

//--- Process the Multi Company checkbox

   cbMultiCpy.Checked := False;

   if edtCurrVersion.Text >= '3.0.1' then
      cbMultiCpy.Visible := True
   else
      cbMultiCpy.Visible := False;

//--- Process the New Style Passwords and New Encode checkboxes

   cbNewPass.Checked   := False;
//   cbNewEncode.Checked := False;

   if edtCurrVersion.Text >= '3.0.2' then begin

      cbNewPass.Visible   := True;
//      cbNewEncode.Visible := True;

   end else begin

      cbNewPass.Visible   := False;
//      cbNewEncode.Visible := False;

   end;

end;

{
//---------------------------------------------------------------------------
// Convert to do new style encoding
//---------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.DoNewEncoding();
type

   REC_Pass_Struct = record
      Password : string;
      Key      : integer;
   end;

var
   idx1               : integer;
   S1, OldStr, NewStr : string;
   PassList           : array of REC_Pass_Struct;
   Pass_Rec           : REC_Pass_Struct;
{$IFDEF WINDOWS}
   RegIni             : TRegistryIniFile;
{$ELSE}
   RegIni             : TINIFile;
{$ENDIF}

begin

//--- Do the new encoding if the option was selected

   if cbNewEncode.Checked = True then begin

      stProgress.Caption := 'Converting to New Style Encoding';
      Application.ProcessMessages;

      S1 := 'SELECT Control_Key, Control_Password FROM control';
      if DM_Put_DB(S1,TYPE_SELECT) = False then begin

         if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click on [Yes] to continue; or' + #10 + #10 + 'Click on [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

//--- Step through each user definition and change the encoding of the password

      SetLength(PassList,SQLQry1.RecordCount);
      SQLQry1.First;

      for idx1 := 1 to SQLQry1.RecordCount do begin

{$IFDEF OLD_ENCODING}
         Pass_Rec.Password := jvCipher.DecodeString(ThisPass,SQLQry1.FieldByName('Control_Password').AsString);
{$ELSE}
         Pass_Rec.Password := Vignere(CYPHER_DEC,SQLQry1.FieldByName('Control_Password').AsString,SecretPhrase);
{$ENDIF}

         Pass_Rec.Key      := SQLQry1.FieldByName('Control_Key').AsInteger;
         Pass_Rec.Password := Vignere(CYPHER_ENC,Pass_Rec.Password,SecretPhrase);

         PassList[idx1] := Pass_Rec;

         SQLQry1.Next();

      end;

//--- Write the transformed passwords to the database

      for idx1 := 1 to SQLQry1.RecordCount do begin

         Pass_Rec := PassList[idx1];
         S1 := 'UPDATE control SET Control_Password = "' + Pass_Rec.Password + '" WHERE Control_Key = ' + IntToStr(Pass_Rec.Key);

         if DM_Put_DB(S1,TYPE_OTHER) = False then begin

            if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click on [Yes] to continue; or' + #10 + #10 + 'Click on [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

         end;

      end;

{$IFDEF WINDOWS}
      RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
      RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}

//--- Transform the value of DBPrefix

{$IFDEF OLD_ENCODING}
      OldStr := jvCipher.DecodeString(ThisPass,RegIni.ReadString('Preferences','DBPrefix','invali'));
      NewStr := jvCipher.EncodeString(ThisPass,OldStr + FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now)));
{$ELSE}
      OldStr := Vignere(CYPHER_DEC,RegIni.ReadString('Preferences','DBPrefix','invali'),SecretPhrase);
      NewStr := Vignere(CYPHER_ENC,(OldStr + MaskField(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now),TYPE_MASK)),SecretPhrase);
{$ENDIF}
{
      OldStr := jvCipher.DecodeString(ThisPass,RegIni.ReadString('Preferences','DBPrefix','invalid'));
      NewStr := Vignere(CYPHER_ENC,OldStr,ThisPass);
      T1     := NewStr.SubString(1,6);
      T2     := NewStr.SubString(7,99);
      NewStr := T1 + EncodePass(T2);
}

      RegIni.WriteString('Preferences','DBPrefix',NewStr);

      RegIni.Destroy;
   end;

end;
}

//------------------------------------------------------------------------------
// Convert to new style passwords. As of LPMS Version 3.1.0 the login passwords
// are encrypted using the Vignere coder/encoder from JEDI/JVCL. The aim here
// is to convert from the JEDI/JVCL version to the BSD Vignere coder/encoder.
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.DoNewStylePass();
type

   REC_Pass_Struct = record
      Password : string;
      Key      : integer;
   end;

var
   idx1       : integer;
   DoNewStyle : boolean = True;
   S1         : string;
   PassList   : array of REC_Pass_Struct;
   Pass_Rec   : REC_Pass_Struct;
{$IFDEF WINDOWS}
   RegIni     : TRegistryIniFile;
{$ELSE}
   RegIni     : TINIFile;
{$ENDIF}

begin

   if cbNewPass.Checked = True then begin

      stProgress.Caption := 'Converting to New Style Passwords';
      Application.ProcessMessages;

      try

{$IFDEF WINDOWS}
         RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
         RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}

//--- Check if the NewStylePassword Key exists and warn user if it does

         if (RegIni.ValueExists('Preferences','NewStylePass') = True) then begin

            if (RegIni.ReadBool('Preferences','NewStylePass',False) = True) then begin

               if Application.MessageBox(PChar('WARNING: New Style Password registry key already exists. Proceeding with a conversion to New Style Passwords may result in an inability to log into LPMS. You can:' + #10 + #10 + #10 + 'Click on [Yes] to proceed; or' + #10 + #10 + 'Click on [No] to skip'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                  DoNewStyle := False;

            end;

         end;

         if DoNewStyle = True then begin

//--- Call an external utility to convert from JEDI/JVCL Vignere to plain text

            //

//--- Now we can proceed to encode the plain text passwords using the BSD
//--- Vignere coder/encoder

            S1 := 'SELECT * FROM control';

            if DM_Put_DB(S1,TYPE_SELECT) = False then begin

               if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click on [Yes] to continue; or' + #10 + #10 + 'Click on [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                  Exit;

            end;

//--- Extract the Passowrd and Key for each record in the control table then
//--- transform the password to a new style password

            SetLength(PassList,SQLQry1.RecordCount);
            SQLQry1.First;

            for idx1 := 1 to SQLQry1.RecordCount do begin

               Pass_Rec.Password := SQLQry1.FieldByName('Control_Password').AsString;
               Pass_Rec.Key      := SQLQry1.FieldByName('Control_Key').AsInteger;
               Pass_Rec.Password := Vignere(CYPHER_ENC,Pass_Rec.Password,SecretPhrase);

               PassList[idx1] := Pass_Rec;

               SQLQry1.Next;

            end;

//--- Write the transformed passwords to the database

            for idx1 := 1 to SQLQry1.RecordCount do begin

               Pass_Rec := PassList[idx1];

               S1 := 'UPDATE control SET Control_Password = "' + Pass_Rec.Password + '" WHERE Control_Key = ' + IntToStr(Pass_Rec.Key);
               if DM_Put_DB(S1,TYPE_OTHER) = False then begin

                  if Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '". You can:' + #10 + #10 + #10 + 'Click on [Yes] to continue; or' + #10 + #10 + 'Click on [No] to abort'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
                     Exit;

               end;

            end;

            RegIni.WriteBool('Preferences','NewStylePass',True);

         end;

      finally

         RegIni.Destroy;

      end;

   end;

end;

//------------------------------------------------------------------------------
// Set up for Multi Company support
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.DoMultiCompany();
var
   idx1, ThisIntVal          : integer;
   ThisFltVal                : double;
   ThisStrVal, LocalDBPrefix : string;
{$IFDEF WINDOWS}
   RegIniSgl,RegIniMul       : TRegistryIniFile;
{$ELSE}
   RegIniSgl, RegIniMul      : TINIFile;
{$ENDIF}
{$INCLUDE 'LPMS_Registry.inc'}

begin

   if cbMultiCpy.Checked = True then begin

      stProgress.Caption := 'Converting to Multi Company';
      Application.ProcessMessages;

      try

{$IFDEF WINDOWS}
         RegIniSgl := TRegistryIniFile.Create('Software\BlueCrane Software\LPMS 3');
         RegIniMul := TRegistryIniFile.Create('Software\BlueCrane Software\LPMS 3\' + edtNewPrefixC.Text);
{$ELSE}
         RegIniSgl := TINIFile.Create(RegPath + 'LPMS 3.ini');
         RegIniMul := TINIFile.Create(RegPath + edtNewPrefixC.Text + '.ini');
{$ENDIF}

//--- Transfer the Character Keys from the Single Location to the Multi location

         for idx1 := 1 to Length(DefChar) do begin

            if RegIniSgl.ValueExists('Preferences',DefChar[idx1]) then begin

               ThisStrVal := RegIniSgl.ReadString('Preferences',DefChar[idx1],'Not Found');
               RegIniMul.WriteString('Preferences',DefChar[idx1],ThisStrVal);

            end;

         end;

//--- Add the Integer Keys to the list

         for idx1 := 1 to Length(DefInt) do begin

            if RegIniSgl.ValueExists('Preferences',DefInt[idx1]) then begin

               ThisIntVal := RegIniSgl.ReadInteger('Preferences',DefInt[idx1],0);
               RegIniMul.WriteInteger('Preferences',DefInt[idx1],ThisIntVal);

            end;

         end;

//--- Add the Float Keys to the list

         for idx1 := 1 to Length(DefFloat) do begin

            if RegIniSgl.ValueExists('Preferences',DefFloat[idx1]) then begin

               ThisFltVal := RegIniSgl.ReadFloat('Preferences',DefFloat[idx1],0.00);
               RegIniMul.WriteFloat('Preferences',DefFloat[idx1],ThisFltVal);

            end;

         end;

//--- Insert the Multi Company flag

         RegIniSgl.WriteString('Preferences','MultiCompany','1');

//--- Insert DBPrefix for the new Multi Company

{$IFDEF OLD_ENCODING}
         LocalDBPrefix := jvCipher.EncodeString(ThisPass,(edtNewPrefixC.Text + MaskField(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now),TYPE_MASK)));
{$ELSE}
         LocalDBPrefix := Vignere(CYPHER_ENC,edtNewPrefixC.Text,SecretPhrase) + MaskField(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now),TYPE_MASK);
{$ENDIF}

         RegIniMul.WriteString('Preferences','DBPrefix',LocalDBPrefix);

      finally

         RegIniSgl.Destroy;
         RegIniMul.Destroy;

      end;

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
//      Process.Parameters.Add('--args');
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
// User clicked on the Lock/Unlock button on the SQL Page
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

      edtHostNameD.Clear;
      edtUserNameD.Clear;
      edtPasswordD.Clear;
      edtPrefixD.Clear;
      edtSQLD.Clear;

   end;

   pgTabsChange(Sender);

end;

//------------------------------------------------------------------------------
// User click on the Process button on the SQL Page
//------------------------------------------------------------------------------
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

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Select the Database to use

   S1 := 'USE ' + edtPrefixD.Text + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

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
   idx1, idx2, Len, ThisTop : integer;
   Str                      : string;

begin

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
{--- Restore Tab functions                                                  ---}
{==============================================================================}

//------------------------------------------------------------------------------
// User clicked on the button embedded in the Backup File field
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtBackupFileButtonClick(Sender: TObject);
begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

   edtBackupFile.Filter      := 'LPMS Backup Files (*.lpb)|*.lpb|All Files (*.*)|*.*';
   edtBackupFile.DefaultExt  := '.lpb';
   edtBackupFile.FilterIndex := 1;

end;

//------------------------------------------------------------------------------
// User selected a Backup File name
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtBackupFileChange(Sender: TObject);
var
   idx1     : integer;
   ThisList : TListItem;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

//--- Check whether the supplied file exists

   if FileExists(edtBackupFile.Text) = False then
      Exit;

   GetList(edtBackupFile.Text);

//--- Check whether the Backup is a valid backup file

   if (((edtTitle.Text = 'LPMS_Backup') and (edtVersion.Text >= '3.0.2')) or (edtTitle.Text = 'BSD_Backup_Manager') or (edtTitle.Text = 'BSD Backup Manager')) then
      cbType.Checked := True
   else begin

      Application.MessageBox(PChar('"' + edtBackupFile.Text + '" appears not to be a valid backup file.'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   if edtMode.Text = 'Managed' then begin

      stMsgB.Caption  := 'LPMS Utility: Select "Full Restore" to restore all tables listed below or select "Choose what to Restore" to do a selective restore of the tables listed below. Select "Replace content" to replace the current content of the selected databases. WARNING: This action will destroy the current content of the database and cannot be reversed once started.';

      rbFull.Enabled     := True;
      rbPartial.Enabled  := True;
      cbType.Enabled     := True;

   end else
      stMsgB.Caption    := 'LPMS Utility: Click [Restore] to proceed with the Restore or [Cancel] to cancel the Restore request. WARNING: This action will destroy the current content of the tables listed below and cannot be reversed once started.';

   edtTitle.Enabled   := True;
   edtDate.Enabled    := True;
   edtTime.Enabled    := True;
   edtVersion.Enabled := True;
   edtMode.Enabled    := True;
   edtType.Enabled    := True;

//--- Extract the tables that are in scope

   lvTables.Clear;

   for idx1 := 0 to TableList.Count - 1 do begin

      ThisList := lvTables.Items.Add;

      ThisList.Caption := TableList.Strings[idx1];
      ThisList.Checked := True;

   end;

   rbFullClick(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on Full Restore on the Restore Tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.rbFullClick(Sender: TObject);
var
   idx1 : integer;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

   lvTables.Enabled := False;
   btnAllB.Enabled  := False;

   for idx1 := 0 to lvTables.Items.Count - 1 do
      lvTables.Items.Item[idx1].Checked := True;

   btnCancelB.Default  := False;
   btnProcessB.Default := True;
   btnProcessB.Enabled := True;

end;

//------------------------------------------------------------------------------
// User clicked on Choose what to Restore on the Restore tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.rbPartialClick(Sender: TObject);
var
   idx1  : integer;
   Found : boolean = False;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

   lvTables.Enabled := True;
   btnAllB.Enabled  := True;

//--- Check whether any items have been selected

   for idx1 := 0 to lvTables.Items.Count - 1 do begin

      if lvTables.Items.Item[idx1].Checked = True then begin

         Found := True;
         break;

      end;

   end;

   btnProcessB.Enabled := Found;
   btnCancelB.Default  := not Found;
   btnProcessB.Default := Found;

end;

//------------------------------------------------------------------------------
// User clicked on the Restore button on the Restore tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnProcessBClick(Sender: TObject);
var
   idx1, idx2, idx3  : integer;
   RecCount          : integer = 0;
   DoLoop            : boolean;
   Found             : boolean = False;
   S1, ThisTable     : string;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

//--- Check whether any items have been selected

   for idx1 := 0 to lvTables.Items.Count - 1 do begin

      if lvTables.Items.Item[idx1].Checked = True then begin

         Found := True;
         break;

      end;

   end;

   if Found = False then
      Exit;

//--- Select the MySQL server on which the information will be restored

   DoRestore := False;

   FLPMS_UtilityApp.Hide;
   FLPMS_UtilitySelDB := TFLPMS_UtilitySelDB.Create(Application);
   FLPMS_UtilitySelDB.ShowModal;
   FLPMS_UtilitySelDB.Destroy;
   FLPMS_UtilityApp.Show;

   if DoRestore = False then
      Exit;

//--- If we get here we can proceed with the Restore. Connect to the Database
//--- using the credentials provided by the user.

   SQLCon.HostName     := RestoreHost;
   SQLCon.UserName     := RestoreUser;
   SQLCon.Password     := RestorePass;
   SQLCon.DatabaseName := ThisDBPrefix + '_LPMS';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;
   SQLDs1.DataSet      := SQLQry1;

   if DM_Open_Connection = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

//--- Select the database associated with the DBPRefix

   S1 := 'USE ' + ThisDBPrefix + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   if edtMode.Text = 'Standalone' then begin

//--- The backup was taken in Standalone mode - we simply execute each SQL
//--- instruction in the ImportList (loaded from the Backup file) in turn
//--- We start with string 1 as string 0 contains the meta data

//--- Give the user another chance to opt out

      if Application.MessageBox(PChar('WARNING: Backup taken on "' + edtDate.Text + '" at "' + edtTime.Text + '" will overwrite existing information in Database "' + RestoreHost + '[' + ThisDBPrefix + ']".' + #10 + #10 + 'WARNING: This is a permanent operation and cannot be reversed once started. You can:' + #10 + #10 + #10 + 'Click [Yes] to continue with the Restore; or' + #10 + #10 + 'Click on [No] to return.'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
         Exit;
      
      RestoreActive := True;

      stProgressB.Caption := 'Writing Record no.: 1';
      Application.ProcessMessages;

      for idx1 := 1 to ImportList.Count - 1 do begin

         S1 := ImportList.Strings[idx1];

         if DM_Put_DB(S1,TYPE_OTHER) = False then begin

            stProgressB.Caption := 'Writing Record no.: ' + IntToStr(RecCount);
            Application.ProcessMessages;

            Application.MessageBox(PChar('Information message: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));
            RestoreActive := False;
            Exit;

         end;

         Inc(RecCount);

         if (RecCount mod 100) = 0 then begin

            stProgressB.Caption := 'Writing Record no.: ' + IntToStr(RecCount);
            Application.ProcessMessages;

         end;

      end;

   end else begin

//--- The backup was taken in Managed mode - we need to work through the SQL
//--- statements systematically and restore only the tables in the list returned
//--- from FldExpImp. In addition we need to take the value of BackupReplace
//--- into account. If the value is True then we need to DROP and CREATE the
//--- table before restoring.
//--- We start with string 1 as string 0 contains the meta data

//--- Give the user another chance to opt out if 'Replace content' was selected

      if cbType.Checked = True then begin

         if Application.MessageBox(PChar('WARNING: Backup taken on "' + edtDate.Text + '" at "' + edtTime.Text + '" will overwrite existing information in Database "' + RestoreHost + '[' + ThisDBPrefix + ']".' + #10 + #10 + 'WARNING: This is a permanent operation and cannot be reversed once started. You can:' + #10 + #10 + #10 + 'Click [Yes] to continue with the Restore; or' + #10 + #10 + 'Click [No] to return.'),'LPMS Utility',(MB_YESNO + MB_ICONSTOP)) = ID_NO then
            Exit;

      end;

      RestoreActive := True;
      TableList.Clear;

//--- Insert the tables to be restored into the TableList

      for  idx1 := 0 to lvTables.Items.Count - 1 do begin

         if lvTables.Items.Item[idx1].Checked = True then
            TableList.Add(lvTables.Items.Item[idx1].Caption);

      end;

      idx3 := 1;

      for idx1 := 0 to TableList.Count - 1 do begin

         idx2 := FindTable(TableList.Strings[idx1],idx3);

         if idx2 = -1 then begin

            Application.MessageBox(PChar('Unexpected error during Restore - Table "' + TableList.Strings[idx1] + '" is not in the Backup file.' + #10 + #10 + 'Restore process aborted'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
            RestoreActive := False;
            Exit;

         end;

//--- Position at the next line to be pocessed and extract the first character

         idx3 := idx2;

//--- If the first character is a comment then ignore it otherwise assume it
//--- is a line indicatig the table to be restored

         if ImportList.Strings[idx3].Substring(0,1) = '#' then
            break;

         ThisTable := ImportList.Strings[idx3].SubString(1,999999);

         stProgressB.Caption := 'Restoring Table "' + ThisTable + '", Writing Record no.: 1';
         Application.ProcessMessages;

//--- Move to the next line then check if 'Replace content' is unchecked in
//--- which case we need to skip the Drop and Create statements (next 2 lines)

         Inc(idx3);

         if cbType.Checked = False then
            idx3 := idx3 + 2;

//--- From here we process all lines until we reach the next table indicator or
//--- EOF

         DoLoop := True;

         while DoLoop = True do begin

            if ImportList.Strings[idx3].SubString(0,1) = '1' then
               S1 := ImportList.Strings[idx3].SubString(1,999999)
            else begin

               DoLoop := False;
               Continue;

            end;

//--- Carry on and restore this record

            if DM_Put_DB(S1,TYPE_OTHER) = False then begin

               stProgressB.Caption := 'Restoring Table "' + ThisTable + '", Writing Record no.: ' + IntToStr(RecCount);
               Application.ProcessMessages;

               Application.MessageBox(PCHar('Information message: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));
               RestoreActive := False;
               Exit;

            end;

            Inc(RecCount);

            if (RecCount mod 100) = 0 then begin

               stProgressB.Caption := 'Restoring Table "' + ThisTable + '", Writing Record no.: ' + IntToStr(RecCount);
               Application.ProcessMessages;

            end;

            Inc(idx3);

         end;

      end;

   end;

//--- Show the final count

   if edtMode.Text = 'Standalone' then
      stProgressB.Caption := 'Writing Record no.: ' + IntToStr(RecCount)
   else
      stProgressB.Caption := 'Restoring Table "' + ThisTable + '", Writing Record no.: ' + IntToStr(RecCount);

   Application.ProcessMessages;

//--- Close the database

   SQLQry1.Close;
   SQLCon.Close;

   Application.MessageBox(PChar('Restore successfully completed. ' + IntToStr(RecCount) + ' Records restored'),'LPMS Utility',(MB_OK + MB_ICONINFORMATION));
   stProgressB.Caption := '';

   RestoreActive := False;

end;

//------------------------------------------------------------------------------
// Function to locate the start of the passed table in the Restore list
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.FindTable(ThisTable: string; idx1: integer) : integer;
var
   idx2               : integer;
   S0, S1, FoundTable : string;

begin

   Result := -1;

   for idx2 := idx1 to ImportList.Count - 1 do begin

      S0 := ImportList.Strings[idx2].SubString(0,1);
      S1 := ImportList.Strings[idx2].SubString(1,999999);

      if StrToInt(S0) = LINE_TABLE then
         FoundTable := S1;

      if FoundTable = ThisTable then begin
         Result := idx2;
         break;
      end;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Toggle All button on the Restore tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnAllBClick(Sender: TObject);
var
   idx1 : integer;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

   Selected := not Selected;

   for idx1 := 0 to lvTables.Items.Count - 1 do
      lvTables.Items.Item[idx1].Checked := Selected;

   lvTables.Refresh;

end;

//------------------------------------------------------------------------------
// User selected/unselected an item in the tables listview on the Restore tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.lvTablesItemChecked(Sender: TObject; Item: TListItem);
var
   idx1  : integer;
   Found : boolean = False;

begin

//--- Ignore while a Restore is being executed

   if RestoreActive = True then
      Exit;

//--- Check whether any items have been selected

   for idx1 := 0 to lvTables.Items.Count - 1 do begin

      if lvTables.Items.Item[idx1].Checked = True then begin

         Found := True;
         break;

      end;

   end;

   btnProcessB.Enabled := Found;
   btnCancelB.Default  := not Found;
   btnProcessB.Default := Found;

end;

//------------------------------------------------------------------------------
// User clicked on the Lock/Unlock button on the Restore Tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnUnlockBClick(Sender: TObject);
var
   Passwd : string;

begin

   if LockB_State = True then begin

      Passwd := InputQueryM('LPMS Utility','Pass phrase:',ord(TYPE_PASSWORD));

      if Passwd = PassPhrase then begin

         LockB_State           := False;
         edtBackupFile.Enabled := False;

      end;

   end else begin

      LockB_State           := True;
      edtBackupFile.Enabled := True;

   end;

   pgTabsChange(Sender);

end;

//------------------------------------------------------------------------------
// Function to extract meta data from the backup file
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.GetList(FileName: string);
var
   ThisLine          : string;
   FirstLine, Tokens : TStringList;

begin

//--- Load the entire backup file into memory then extract the first line

   ImportList.LoadFromFile(FileName);
   ThisLine := ImportList.Strings[0];

//--- Set up to extract the tokens in the First line

   try

      Tokens := TStringList.Create;
      ExtractStrings(['|'], [], PChar(ThisLine),Tokens);

//--- Extract the values of the individual tokens

      edtTitle.Text   := Tokens.Strings[0].SubString(7,99);
      edtDate.Text    := Tokens.Strings[1].SubString(5,99);
      edtTime.Text    := Tokens.Strings[2].SubString(5,99);
      edtVersion.Text := Tokens.Strings[3].SubString(8,99);
      edtType.Text    := Tokens.Strings[4].SubString(5,99);
      edtMode.Text    := Tokens.Strings[5].SubString(5,99);
      RestoreTables   := Tokens.Strings[6].SubString(7,1024);

//--- Extract the list of tablenames

      FirstLine               := TStringList.Create;
      FirstLine.Delimiter     := ',';
      FirstLine.DelimitedText := RestoreTables;

      TableList.Clear;
      TableList.AddStrings(FirstLine);

   finally

      FirstLine.Free;
      Tokens.Free;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Listview
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.lvLogClick(Sender: TObject);
begin

//--- Check whether anything was selected

   if lvLog.ItemIndex = -1 then
      Exit;

   MoveItems;
   edtUser.SetFocus;

end;

{==============================================================================}
{--- Log Tab functions                                                      ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Common function to facilitate movement through the Billing records
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.MoveItems();
var
   idx, idx2, Dummy : integer;
   ThisItem         : TListItem;

begin

   if lvLog.Items.Count > 0 then begin

      ThisItem := lvLog.Items.Item[lvLog.ItemIndex];
      lvLog.Items.Item[lvLog.ItemIndex].MakeVisible(False);

//--- Populate the fields using the selected record

      edtDateL.Text        := ThisItem.Caption;
      edtTimeL.Text        := ThisItem.SubItems.Strings[0];
      edtUser.Text         := ThisItem.SubItems.Strings[1];
      edtDescriptionL.Text := ThisItem.SubItems.Strings[2];

   end;

   if lvLog.ItemIndex > 0 then begin

      btnFirst.Enabled := True;
      btnPrev.Enabled  := True;

   end else begin

      btnFirst.Enabled := False;
      btnPrev.Enabled  := False;

   end;

   if lvLog.ItemIndex < (lvLog.Items.Count - 1) then begin

      btnNext.Enabled := True;
      btnLast.Enabled := True;

   end else begin

      btnNext.Enabled := False;
      btnLast.Enabled := False;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to go to the first item
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnFirstClick(Sender: TObject);
begin

   if lvLog.Items.Count > 0 then begin

      lvLog.ItemIndex := 0;
      lvLog.Items.Item[lvLog.ItemIndex].Focused;

   end;

   MoveItems;

end;

//------------------------------------------------------------------------------
// User clicked on the button to go to the previous item
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnPrevClick(Sender: TObject);
begin

   if lvLog.Items.Count > 0 then begin

      lvLog.ItemIndex := lvLog.ItemIndex - 1;
      lvLog.Items.Item[lvLog.ItemIndex].Focused;

   end;

   MoveItems;

end;

//------------------------------------------------------------------------------
// User clicked on the button to go to the next item
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnNextClick(Sender: TObject);
begin

   if lvLog.Items.Count > 0 then begin

      lvLog.ItemIndex := lvLog.ItemIndex + 1;
      lvLog.Items.Item[lvLog.ItemIndex].Focused;

   end;

   MoveItems;

end;

//------------------------------------------------------------------------------
// User clicked on the button to go to the last item
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnLastClick(Sender: TObject);
begin

   if lvLog.Items.Count > 0 then begin

      lvLog.ItemIndex := lvLog.Items.Count - 1;
      lvLog.Items.Item[lvLog.ItemIndex].Focused;

   end;

   MoveItems;

end;

//------------------------------------------------------------------------------
// User entered the details for User, Password and Host
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtUserLChange(Sender: TObject);
begin

   if ((Trim(edtUserL.Text) <> '') and (Trim(edtPasswordL.Text) <> '') and (Trim(edtHostL.Text) <>'')) then begin

      btnOpenLog.Enabled  := True;
      btnProcessL.Enabled := True;

   end else begin

      btnOpenLog.Enabled      := False;

      dtpSDate.Enabled        := False;
      dtpSTime.Enabled        := False;
      dtpEDate.Enabled        := False;
      dtpETime.Enabled        := False;
      btnReload.Enabled       := False;

      edtSearchUser.Enabled   := False;
      btnSearchUser.Enabled   := False;
      edtSearchDesc.Enabled   := False;
      btnSearchDesc.Enabled   := False;
      btnSearchBoth.Enabled   := False;
      chkMatchAny.Enabled     := False;

      lvLog.Enabled          := False;
      chkAutoRefresh.Enabled := False;
      speInterval.Enabled    := False;

      btnFirst.Enabled       := False;
      btnPrev.Enabled        := False;
      btnNext.Enabled        := False;
      btnLast.Enabled        := False;

      btnProcessL.Enabled    := False;

   end;

end;

//---------------------------------------------------------------------------
// User clicked on the button to load the current log
//---------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnOpenLogClick(Sender: TObject);
var
   S1 : string;

begin

//--- Connect to the supplied database

   if DM_Connect_DB = False then
      Exit;

//--- Set the Date and Time defaults

   if DateIsSet = False then begin

      if SetDateTimeDef(TYPE_CURRENT) = False then begin

         Application.MessageBox('No log records found - LPMS Utility cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
         Application.Terminate;
         Exit;

      end;

      DateIsSet := True;

   end;

//--- Set up the default search criteria

   SearchBoth := True;
   if ReadAllRecs('%','%',' OR ') = False then begin

      Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));

//--- Recalibrate

      if SetDateTimeDef(TYPE_CURRENT) = False then begin

         Application.MessageBox('No log records found - LPMS Utility cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
         Application.Terminate;
         Exit;

      end;

//--- Set up the default search criteria

      SearchBoth := True;
      if ReadAllRecs('%','%',' OR ') = False then begin

         Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
         Application.Terminate;
         Exit;

      end;

   end;

   btnLastClick(Sender);

//--- Enable the fields that may now be used

   dtpSDate.Enabled  := True;
   dtpSTime.Enabled  := True;
   dtpEDate.Enabled  := True;
   dtpETime.Enabled  := True;
   btnReload.Enabled := True;

   edtSearchUser.Enabled := True;
   btnSearchUser.Enabled := True;
   edtSearchDesc.Enabled := True;
   btnSearchDesc.Enabled := True;
   btnSearchBoth.Enabled := True;
   chkMatchAny.Enabled   := True;

   lvLog.Enabled          := True;
   chkAutoRefresh.Enabled := True;
   speInterval.Enabled    := True;

//--- Clear the Archive File input field - this will automatically turn off
//--- ArchiveActive

   edtArchive.Clear;

end;

//------------------------------------------------------------------------------
// User clicked on the Load button on he Display Log Pag
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnProcessLClick(Sender: TObject);
begin

//--- If ArchiveActive is True then we should possibly load an archive file,
//--- however if any of the User, Password or Host fields are not empty then
//--- we rather will load the current log

   if ((ArchiveActive = True) and ((Trim(edtUserL.Text) = '') or (Trim(edtPasswordL.Text) = '') or (Trim(edtHostL.Text) = ''))) then begin

      if Trim(edtArchive.Text) <> '' then
         edtArchiveButtonClick(Sender);

   end else begin

      if ((Trim(edtUserL.Text) <> '') and (Trim(edtPasswordL.Text) <> '') and (Trim(edtHostL.Text) <> '')) then
         btnOpenLogClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Lock/Unlock button on the Display Log Tab
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnUnlockLClick(Sender: TObject);
var
   Passwd : string;

begin

//--- Check whether we can do the unlock - if edtSearchUser is disabled then
//--- the ListView content is not valid

   if LockL_State = True then begin

      if edtSearchUser.Enabled = False then
         Exit;

   end;

//--- Now go ahead and do the Lock/Unlock

   if LockL_State = True then begin

      Passwd := InputQueryM('LPMS Utility','Pass phrase:',ord(TYPE_PASSWORD));

      if Passwd = PassPhrase then begin

         LockL_State        := False;
         btnLockL.Visible   := False;
         btnUnlockL.Visible := True;

      end;

   end else begin

      LockL_State        := True;
      btnLockL.Visible   := True;
      btnUnlockL.Visible := False;

   end;

   btnAll.Enabled     := not LockL_State;
   btnArchive.Enabled := not LockL_State;

end;

//------------------------------------------------------------------------------
// User clicked on the button embedded in the Archive File field
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtArchiveButtonClick(Sender: TObject);
begin

   edtArchive.Filter      := 'LPMS Log Archive (*.lla)|*.lla|All Files (*.*)|*.*';
   edtArchive.DefaultExt  := '.lla';
   edtArchive.FilterIndex := 1;

end;

//------------------------------------------------------------------------------
// User entered a Log Archive file name
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtArchiveChange(Sender: TObject);
begin

   if Trim(edtArchive.Text) = '' then
      ArchiveActive := False
   else
      ArchiveActive := True;

end;

//------------------------------------------------------------------------------
// User selected an Archive File
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.edtArchiveAcceptFileName(Sender: TObject; var Value: String);
begin

   edtArchive.Text := Value;

//--- Set the Date and Time defaults

   if DateIsSet = False then begin

      if SetDateTimeDef(TYPE_ARCHIVE) = False then begin

         Application.MessageBox(PChar('No log records found. You can:' + #10 + #10 + #10 + 'Select a different Log Archive; or' + #10 + #10 + 'Aadjust the Start and End Dates/Times'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

      DateIsSet := True;

   end;

//--- Set up the default search criteria

   SearchBoth := True;

   if ReadAllLogRecs('%','%',' OR ') = False then begin

      Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));

//--- Recalibrate

      if SetDateTimeDef(TYPE_ARCHIVE) = False then begin

         Application.MessageBox(PChar('No log records found. You can:' + #10 + #10 + #10 + 'Select a different Log Archive; or' + #10 + #10 + 'Adjust the Start and End Dates/Times'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

//--- Set up the default search criteria

      SearchBoth := True;

      if ReadAllLogRecs('%','%',' OR ') = False then begin

         Application.MessageBox(PChar('No log records found. You can:' + #10 + #10 + #10 + 'Select a different Log Archive; or' + #10 + #10 + 'Adjust the Start and End Dates/Times'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

   end;

   btnLastClick(Sender);

   DateIsSet := False;

//--- Enable the fields that may now be used

   dtpSDate.Enabled       := True;
   dtpSTime.Enabled       := True;
   dtpEDate.Enabled       := True;
   dtpETime.Enabled       := True;
   btnReload.Enabled      := True;

   edtSearchUser.Enabled  := True;
   btnSearchUser.Enabled  := True;
   edtSearchDesc.Enabled  := True;
   btnSearchDesc.Enabled  := True;
   btnSearchBoth.Enabled  := True;
   chkMatchAny.Enabled    := True;

   lvLog.Enabled          := True;
   chkAutoRefresh.Enabled := True;
   speInterval.Enabled    := True;

//--- Disable AutoRefresh when displaying an archive

   chkAutoRefresh.Enabled := False;
   chkAutoRefresh.Checked := False;

//--- Enable the Process buttons

   btnProcessL.Enabled    := True;

end;

//------------------------------------------------------------------------------
// User clicked on the Reload button
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnReloadClick(Sender: TObject);
begin

   if ArchiveActive = False then begin

//--- Connect to the supplied database

      if DM_Connect_DB = False then
         Exit;

      edtSearchUser.Text  := '';
      edtSearchDesc.Text  := '';
      SearchUser          := False;
      SearchDesc          := False;
      SearchBoth          := False;
      chkMatchAny.Checked := False;

//--- Set up the default search criteria

      SearchBoth := True;

      if ReadAllRecs('%','%',' OR ') = False then begin

         Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));

//--- Recalibrate

         if SetDateTimeDef(TYPE_CURRENT) = False then begin

            Application.MessageBox('No log records found - LPMS Utility cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

//--- Set up the default search criteria

         SearchBoth := True;

         if ReadAllRecs('%','%',' OR ') = False then begin

            Application.MessageBox('No log records found - LPMS Utility cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

      end;

      btnLastClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to filter by User
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnSearchUserClick(Sender: TObject);
begin

   if ArchiveActive = False then begin

//--- Connect to the supplied database

      if DM_Connect_DB = False then
         Exit;

      SearchBoth := False;
      SearchDesc := False;
      SearchUser := True;

//--- Filter records by the value supplied in the SearchUser field. If the
//--- search fails then display a message and do another search on all records

      if ReadAllRecs('%' + edtSearchUser.Text + '%','%','') = False then begin

         Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));

//--- Recalibrate

         if SetDateTimeDef(TYPE_CURRENT) = False then begin

            Application.MessageBox('No log records found - FirstRun cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

//--- Set up the default search criteria

         SearchBoth := True;

         if ReadAllRecs('%','%',' OR ') = False then begin

            Application.MessageBox('No log records found - FirstRun cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

      end;

      btnLastClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to filter by Description
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnSearchDescClick(Sender: TObject);
begin

   if ArchiveActive = False then begin

//--- Connect to the supplied database

      if DM_Connect_DB = False then
         Exit;

      SearchBoth := False;
      SearchUser := False;
      SearchDesc := True;

//--- Filter records by the value supplied in the SearchDesc field. If the
//--- search fails then display a message and do another search on all records

      if ReadAllRecs('%','%' + edtSearchDesc.Text + '%','') = False then begin

         Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));

//--- Recalibrate

         if SetDateTimeDef(TYPE_CURRENT) = False then begin

            Application.MessageBox('No log records found - FirstRun cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

//--- Set up the default search criteria

         SearchBoth := True;

         if ReadAllRecs('%','%',' OR ') = False then begin

            Application.MessageBox('No log records found - FirstRun cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

      end;

      btnLastClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to filter by both user and description
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnSearchBothClick(Sender: TObject);
var
   S2 : string;

begin

   if ArchiveActive = False then begin

      SearchDesc := False;
      SearchUser := False;
      SearchBoth := True;

      if chkMatchAny.Checked = True then
         S2 := ' OR '
      else
         S2 := ' AND ';

//--- Connect to the supplied database

      if DM_Connect_DB = False then
         Exit;

//--- Filter records by the value supplied in both the SearchUser field and the
//--- SearchDesc field. If the search fails then display a message and do
//--- another search on all records

      if ReadAllRecs('%' + edtSearchUser.Text + '%','%' + edtSearchDesc.Text + '%',S2) = False then begin

         Application.MessageBox('No log records found - Click OK to continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));

//--- Recalibrate

         if SetDateTimeDef(TYPE_CURRENT) = False then begin

            Application.MessageBox('No log records found - FirstRun cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

//--- Set up the default search criteria

         SearchBoth := True;

         if ReadAllRecs('%','%',' OR ') = False then begin

            Application.MessageBox('No log records found - FirstRun cannot continue...','LPMS Utility',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;

         end;

      end;

      btnLastClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// The user selected/deseleted the checkbox to refresh the log display
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.chkAutoRefreshClick(Sender: TObject);
begin

   if chkAutoRefresh.Checked = True then begin

      timTimer.Interval := speInterval.Value * 1000;
      timTimer.Enabled  := True;

   end else
      timTimer.Enabled := False;

end;

//------------------------------------------------------------------------------
// The Timer to update the log display popped
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.timTimerTimer(Sender: TObject);
begin

   DateIsSet := False;
   btnOpenLogClick(Sender);

end;

//------------------------------------------------------------------------------
// User adjusted the auto refresh interval. Toggle the AutoRefresh setting
// to ensure the new interval is activated
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.speIntervalChange(Sender: TObject);
begin

   chkAutoRefresh.Checked := not chkAutoRefresh.Checked;
   chkAutoRefresh.Checked := not chkAutoRefresh.Checked;

end;

//------------------------------------------------------------------------------
// User clicked on the Select/Deselect All button next to the ListView
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnAllClick(Sender: TObject);
var
   idx : integer;

begin

   Sema := not Sema;

   for idx := 0 to lvLog.Items.Count - 1 do
      lvLog.Items.Item[idx].Checked := Sema;

   lvLog.Refresh;

end;

//------------------------------------------------------------------------------
// User clicked on the Archive button next to the ListView
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.btnArchiveClick(Sender: TObject);
var
   idx1, SaveIdx          : integer;
   ThisSel                : boolean = False;
   LogFile                : TextFile;
   S1, ThisLine, FileName : string;
   SaveList               : TStringList;
begin

//--- We cannot do an archive if the ListView contains the contents of an Archive

   if ArchiveActive = True then
      Exit;

//--- Check whether anything is selected

   for idx1 := 0 to lvLog.Items.Count - 1 do begin

      if lvLog.Items.Item[idx1].Checked = True then begin

         ThisSel := True;
         break;

      end;

   end;

   if ThisSel = False then
      Exit;

//--- If we get here then there is at least one log record selected

   sdArchive.FileName := FormatDateTime('yyyyMMdd',Now) + ' - LPMS Log Archive (' + ThisDBPrefix + ').lla';

   if sdArchive.Execute = True then
      FileName := sdArchive.FileName
   else
      Exit;

   AssignFile(LogFile,FileName);
   ReWrite(LogFile);

   try

      SaveList := TStringList.Create;

      for idx1 := 0 to lvLog.Items.Count - 1 do begin

         if lvLog.Items.Item[idx1].Checked = True then begin

            stMsg.Caption := 'Processing: "' + lvLog.Items.Item[idx1].Caption + ' ' + lvLog.Items.Item[idx1].SubItems.Strings[0] + ' ' + lvLog.Items.Item[idx1].SubItems.Strings[1] + ' ' + lvLog.Items.Item[idx1].SubItems.Strings[2] + '"';
            Application.ProcessMessages;

            SaveList.Add(lvLog.Items.Item[idx1].Caption);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[0]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[1]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[2]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[3]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[4]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[5]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[6]);
            SaveList.Add(lvLog.Items.Item[idx1].SubItems.Strings[7]);

            ThisLine := Assemble(SaveList);
            WriteLn(LogFile,ThisLine);
            SaveList.Clear;

         end;

      end;

   //--- Close the archive file and delete the dynamic StringList

      CloseFile(LogFile);

   finally

      SaveList.Free;

   end;

//--- Now delete the checked records then refresh the display
//--- Connect to the supplied database

   if DM_Connect_DB = False then
      Exit;

   for idx1 := 0 to lvLog.Items.Count - 1 do begin

      if lvLog.Items.Item[idx1].Checked = True then
         DeleteItem(StrToInt(lvLog.Items.Item[idx1].SubItems.Strings[7]));

   end;

   SaveIdx   := lvLog.ItemIndex;
   DateIsSet := False;
   btnOpenLogClick(Sender);

   if SaveIdx >= lvLog.Items.Count then
      lvLog.ItemIndex := lvLog.Items.Count - 1
   else
      lvLog.ItemIndex := SaveIdx;

   lvLogClick(Sender);

end;

(******************************************************************************)

{==============================================================================}
{--- Database functions                                                     ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Function used by Log Display to connect to a datastore
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.DM_Connect_DB() : boolean;
var
   S1 : string;

begin

//--- Connect to the supplied host

   SQLCon.HostName     := edtHostL.Text;
   SQLCon.UserName     := edtUserL.Text;
   SQLCon.Password     := edtPasswordL.Text;
   SQLCon.DatabaseName := 'mysql';
   SQLTran.DataBase    := SQLCon;
   SQLQry1.Transaction := SQLTran;
   SQLDs1.DataSet      := SQLQry1;

   if DM_Open_Connection() = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Result := False;

   end;

//--- Select the database associated with the DBPRefix

   S1 := 'USE ' + ThisDBPrefix + '_LPMS';
   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Result := False;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Function to open a connection to the datastore
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.DM_Open_Connection() : boolean;
begin

   try

      SQLQry1.Close;
      SQLCon.Close;

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

//---------------------------------------------------------------------------
// Function to insert the LPMS signature and the Default Administrator
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.DM_PutSignature() : boolean;
var
   S1, ThisVersion : string;

begin

   ThisVersion := '3.2.1';
   Result := False;

   S1 := 'INSERT INTO lpms (Signature, Version, VATRegistered, UniqueNum, CpyName, Create_By, Create_On, Create_At) Values("Legal Practise Management System - LPMS", "' +
          ThisVersion + '", 0, 1, "' + edtCpyName.Text + '", "' +
          edtUserName.Text + '", "' +
          FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) + '", "' +
          FormatDateTime(DefaultFormatSettings.LongTimeFormat,Now) + '")';

   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   S1 := 'INSERT INTO control (Control_UserID, Control_Password, Control_Name, Control_UserType, Control_FeeEarner, Control_Unique, Create_By, Create_Date, Create_Time) Values("Administrator", "ƒÐâÎ±ÛÔâ×ÇÞØ", "Default System Administrator", "Administrator", 0, 1, "' +
          edtUserName.Text + '", "' +
          FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now) + '", "' +
          FormatDateTime(DefaultFormatSettings.LongTimeFormat,Now) + '")';

   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Function to delete a given log record (used after successful archive)
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.DeleteItem(Key: integer) : boolean;
var
   S1 : string;

begin

   S1 := 'DELETE FROM log WHERE Log_Key = ' + IntToStr(Key);

//--- Delete the Record

   if DM_Put_DB(S1,TYPE_OTHER) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Result := False;
      Exit

   end;

   Result := True;

end;

(******************************************************************************)

{==============================================================================}
{--- Support functions                                                      ---}
{==============================================================================}

//------------------------------------------------------------------------------
// Function to set up the default search criteria on the Log Display tab
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.SetDateTimeDef(ThisType: integer) : boolean;
var
   NumLines              : integer = 0;
   LogLine, S1, FileName : string;
   LogFile               : TextFile;

begin

   if ThisType = TYPE_ARCHIVE then begin

//--- Read the the first record from the Archive

      AssignFile(LogFile,edtArchive.Text);
      Reset(LogFile);


      ReadLn(LogFile,LogLine);

      if Trim(LogLine) <> '' then begin

         LogList := Disassemble(LogLine,TYPE_PLAIN);

         dtpSDate.Date := StrToDate(LogList.Strings[0]);
         dtpSTime.Time := StrToTime(LogList.Strings[1]);
         dtpEDate.Date := Now;
         dtpETime.Time := Now;

         Inc(NumLines);

      end;


      CloseFile(LogFile);

      if NumLines = 0 then
         Result := False;

   end else begin

//--- Read all records from the log to determine the date of the first record

      S1 := 'SELECT Log_Date FROM log ORDER BY Log_Date ASC';

      if DM_Put_DB(S1,TYPE_SELECT) = False then begin

         Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
         Result := False;

      end;

//--- If the log is empty then we must return to avoid errors

      if SQLQry1.RecordCount = 0 then
         Result := False;

//--- Get the date of the first log record

      SQLQry1.First;

      dtpSDate.Date := StrToDate(SQLQry1.FieldByName('Log_Date').AsString);
      dtpSTime.Time := StrToTime('00:00:00');
      dtpEDate.Date := Now;
      dtpETime.Time := Now;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Function to read all the log records into the ListView. S2 indicates
// whether the filter should match both User and Description
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.ReadAllRecs(UserStr,DescStr,S2: string) : boolean;
var
   S1, S3, SDateTime, EDateTime : string;
   ThisList                     : TListItem;

begin

//--- Prepare the Date and Time strings for demarcating the information to show

   SDateTime := FormatDateTime(DefaultFormatSettings.ShortDateFormat,dtpSDate.Date) + FormatDateTime(DefaultFormatSettings.LongTimeFormat,dtpSTime.Time);
   EDateTime := FormatDateTime(DefaultFormatSettings.ShortDateFormat,dtpEDate.Date) + FormatDateTime(DefaultFormatSettings.LongTimeFormat,dtpETime.Time);

//--- Determine whether we should filter on User, Description or Both

   if SearchBoth = True then begin

      S3 := '(Log_User LIKE "' + UserStr + '"' + S2 + 'Log_Activity LIKE "' +
            DescStr + '")';

   end else if SearchUser = True then begin

      S3 := '(Log_User LIKE "' + UserStr + '")';

   end else begin

      S3 := '(Log_Activity LIKE "' + DescStr + '")';

   end;

   SearchBoth := False;
   SearchUser := False;
   SearchDesc := False;

//--- Load the data

   S1 := 'SELECT * FROM log WHERE (CONCAT(Log_Date, Log_Time) >= "' +
         SDateTime + '" AND CONCAT(Log_Date, Log_Time) <= "' + EDateTime +
         '") AND ' + S3 + ' ORDER BY CONCAT(Log_Date, Log_Time) ASC';

   if DM_Put_DB(S1,TYPE_SELECT) = False then begin

      Application.MessageBox(PChar('Unexpected error: "' + ErrMsg + '"'),'LPMS Utility',(MB_OK + MB_ICONSTOP));
      Result := False;

   end;

//--- Return False if no records were found

   if SQLQry1.RecordCount = 0 then
      Result := False;

   SQLQry1.First;
   lvLog.Clear;
   Sema := False;

   while SQLQry1.Eof = False do begin

      ThisList := lvLog.Items.Add;

      ThisList.Caption := SQLQry1.FieldByName('Log_Date').AsString;
      ThisList.SubItems.Add(SQLQry1.FieldByName('Log_Time').AsString);
      ThisList.SubItems.Add(ReplaceQuote(SQLQry1.FieldByName('Log_User').AsString,TYPE_READ));
      ThisList.SubItems.Add(ReplaceQuote(SQLQry1.FieldByName('Log_Activity').AsString,TYPE_READ));
      ThisList.SubItems.Add(ReplaceQuote(SQLQry1.FieldByName('Create_By').AsString,TYPE_READ));
      ThisList.SubItems.Add(SQLQry1.FieldByName('Create_Date').AsString);
      ThisList.SubItems.Add(SQLQry1.FieldByName('Create_Time').AsString);
      ThisList.SubItems.Add(ReplaceQuote(SQLQry1.FieldByName('TimeStamp').AsString,TYPE_READ));
      ThisList.SubItems.Add(SQLQry1.FieldByName('Log_Key').AsString);

      SQLQry1.Next;

   end;

   if lvLog.Items.Count = 1 then
      S1 := ''
   else
      S1 := 's';

   stMsg.Caption := IntToStr(lvLog.Items.Count) + ' Log Record' + S1 + ' in Current Log';
   Result := True;

end;

//------------------------------------------------------------------------------
// Function to read all the log records from an Archive into the ListView.
// S2 indicates whether the filter should match both User and Description
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.ReadAllLogRecs(UserStr,DescStr,S2: string) : boolean;
var
   NumLines                                        : integer = 0;
   LogLine, SDateTime, EDateTime, ThisDateTime, S1 : string;
   LogFile                                         : TextFile;
   ThisList                                        : TListItem;

begin

   if FileExists(edtArchive.Text) = False then
      Exit;

//--- Prepare the Date and Time strings for demarcating the information to show

   SDateTime := FormatDateTime(DefaultFormatSettings.ShortDateFormat,dtpSDate.Date) + FormatDateTime(DefaultFormatSettings.LongTimeFormat,dtpSTime.Time) + '.000';
   EDateTime := FormatDateTime(DefaultFormatSettings.ShortDateFormat,dtpEDate.Date) + FormatDateTime(DefaultFormatSettings.LongTimeFormat,dtpETime.Time) + '.999';

//--- Read the archived records into the ListView

   AssignFile(LogFile,edtArchive.Text);
   Reset(LogFile);

   lvLog.Clear;
   Sema := False;

   while not EOF(LogFile) do begin

      ReadLn(LogFile,LogLine);
      LogList := Disassemble(LogLine,TYPE_PLAIN);

//--- Build the Date and Time constraints

      ThisDateTime := LogList.Strings[0] + LogList.Strings[1];

      if ((ThisDateTime >= SDateTime) and (ThisDateTime <= EDateTime)) then begin

         ThisList := lvLog.Items.Add();

         ThisList.Caption := LogList.Strings[0];
         ThisList.SubItems.Add(LogList.Strings[1]);
         ThisList.SubItems.Add(ReplaceQuote(LogList.Strings[2],TYPE_READ));
         ThisList.SubItems.Add(ReplaceQuote(LogList.Strings[3],TYPE_READ));
         ThisList.SubItems.Add(ReplaceQuote(LogList.Strings[4],TYPE_READ));
         ThisList.SubItems.Add(LogList.Strings[5]);
         ThisList.SubItems.Add(LogList.Strings[6]);
         ThisList.SubItems.Add(ReplaceQuote(LogList.Strings[7],TYPE_READ));
         ThisList.SubItems.Add(LogList.Strings[8]);

         Inc(NumLines);

      end;

      LogList.Free;

   end;

   CloseFile(LogFile);

   if NumLines = 0 then
      Result := False;

   if lvLog.Items.Count = 1 then
      S1 := ''
   else
      S1 := 's';

   stMsg.Caption := IntToStr(lvLog.Items.Count) + ' Log Record' + S1 + ' in "' + edtArchive.Text + '"';
   Result := True;

end;

//------------------------------------------------------------------------------
// Function to Request a PassPhrase from the User
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.InputQueryM(ThisCap, Question: string; DispType: integer) : string;
begin

   FLPMS_InputQuery := TFLPMS_InputQuery.Create(Application);

   FLPMS_InputQuery.Caption := ThisCap;
   FLPMS_InputQuery.edtInput.Clear;
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
// Process the response from the Server
//------------------------------------------------------------------------------
procedure TFLPMS_UtilityApp.ProcessResponse(Response: string);
var
   idx1, MsgType      : integer;
   ThisPrefix, T1, T2 : string;
{$IFDEF WINDOWS}
   RegIni             : TRegistryIniFile;
{$ELSE}
   RegIni             : TINIFile;
{$ENDIF}

begin

//--- Disassemble the response from the Server

   RespList := TStringList.Create;
   RespList := Disassemble(Response,TYPE_CODED);

   if ((RespList.Strings[0] = '0') or (RespList.Strings[0] = '1')) then begin

      idx1 := 1;

      if RespList.Strings[0] = '0' then
         MsgType := MB_ICONINFORMATION
      else
         MsgType := MB_ICONSTOP;

//--- Process the response(s)

      while idx1 < RespList.Count do begin

         case StrToInt(Resplist.Strings[idx1]) of
            1: begin

{$IFDEF WINDOWS}
                  RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
                  RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}
                  RegIni.WriteString('Preferences',RespList.Strings[idx1 + 1],RespList.Strings[idx1 + 2]);
                  RegIni.Destroy;

                  idx1 := idx1 + 3;

            end;

            2: begin

               Application.MessageBox(PChar(RespList.Strings[idx1 + 1]),'LPMS Utility',(MB_OK + MsgType));
               idx1 := idx1 + 2;

            end;

         end;

      end;

//--- Retrieve the newly generated Key from the Registry if registration was successful

      if (RespList.Strings[0] = '0') then begin

{$IFDEF WINDOWS}
         RegIni := TRegistryIniFile.Create(KeepRegString);
{$ELSE}
         RegIni := TINIFile.Create(KeepRegString);
{$ENDIF}

         ThisPrefix := RegIni.ReadString('Preferences','DBPrefix','');
         UserKey    := RegIni.ReadString('Preferences','Key','');

{$IFDEF OLD_ENCODING}
         DBPrefix := jvCipher.EncodeString(ThisPass,(edtPrefix.Text + FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now())));
{$ELSE}
         DBPrefix := Vignere(CYPHER_ENC,edtPrefix.Text,SecretPhrase) + MaskField(FormatDateTime(DefaultFormatSettings.ShortDateFormat,Now),TYPE_MASK);
{$ENDIF}

         RegIni.WriteString('Preferences','DBPrefix',DBPrefix);

         RegIni.Destroy;

         edtPrefix.Text := ThisPrefix;
         edtKey.Text    := UserKey;

      end;

   end;

   RespList.Free;

end;

//------------------------------------------------------------------------------
// Function to manage replacement of single quotes to avoid SQL errors
//------------------------------------------------------------------------------
function TFLPMS_UtilityApp.ReplaceQuote(S1: string; ThisType: integer) : string;
begin

   if ThisType = TYPE_READ then begin

      S1 := AnsiReplaceStr(S1,'&quot','''');
      S1 := AnsiReplaceStr(S1,'&slash','\');
      S1 := AnsiReplaceStr(S1,'§', '\');        // Retained for backwards compatibility

   end else begin

      S1 := AnsiReplaceStr(S1,'''','&quot');
      S1 := AnsiReplaceStr(S1,'\','&slash');

   end;

   Result := S1;

end;

//---------------------------------------------------------------------------
// Function to Assemble a Log entry
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.Assemble(List: TStringList) : string;
var
   idx   : integer;
   Delim : string = '|';
   Str   : string = '';

begin

   for idx := 0 to List.Count - 1 do begin

      Str := Str + List.Strings[idx];
      Str := Str + Delim;

   end;

   Result := Str;

end;

//---------------------------------------------------------------------------
// Function to Disassemble a message or Log entry
//---------------------------------------------------------------------------
function TFLPMS_UtilityApp.Disassemble(Str: string; ThisType: integer) : TStringList;
var
   ThisStr : string;
   Tokens  : TStringList;

begin

   if ThisType = TYPE_CODED then
      ThisStr := Vignere(CYPHER_DEC,Str,SecretPhrase)
   else
      ThisStr := Str;

   Tokens := TStringList.Create;
   ExtractStrings(['|'], [], PChar(ThisStr),Tokens);
   Result := Tokens;

end;

//------------------------------------------------------------------------------
end.

