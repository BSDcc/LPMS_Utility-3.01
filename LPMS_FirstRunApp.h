//---------------------------------------------------------------------------

#ifndef LPMS_FirstRunAppH
#define LPMS_FirstRunAppH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <ADODB.hpp>
#include <DB.hpp>
#include <Dialogs.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
#include <msxmldom.hpp>
#include <XMLDoc.hpp>
#include <xmldom.hpp>
#include <XMLIntf.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <Mask.hpp>
#include "AdvSpin.hpp"
#include "AdvDirectoryEdit.hpp"
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
#include "JvCipher.hpp"
#include "JvComponentBase.hpp"
#include <DBCtrls.hpp>
//---------------------------------------------------------------------------
class TFLPMS_FirstRunApp : public TForm
{
__published:	// IDE-managed Components
   TPageControl *pgTabs;
   TTabSheet *Register;
   TLabel *Label16;
   TLabel *Label18;
   TLabel *Label20;
   TLabel *Label21;
   TLabel *Label22;
   TLabel *Label23;
   TImage *Image3;
   TLabel *Label8;
   TStaticText *StaticText4;
   TEdit *edtName;
   TButton *btnCancelR;
   TButton *btnRegister;
   TEdit *edtEmail;
   TEdit *edtNum;
   TEdit *edtUnique;
   TEdit *edtKey;
   TEdit *edtPrefix;
   TEdit *edtCompany;
   TTabSheet *Setup;
   TLabel *Label1;
   TLabel *Label2;
   TLabel *Label3;
   TLabel *Label4;
   TLabel *Label5;
   TImage *Image2;
   TLabel *Label7;
   TButton *btnCancelF;
   TButton *btnProcessF;
   TEdit *edtHostName;
   TEdit *edtPassword;
   TEdit *edtPrefixF;
   TEdit *edtUserName;
   TStaticText *StaticText1;
   TEdit *edtCpyName;
   TTabSheet *Maintenance;
   TLabel *Label6;
   TLabel *Label14;
   TStaticText *StaticText2;
   TEdit *edtPrefixM;
   TButton *btnCancelM;
   TButton *btnProcessM;
   TIdTCPClient *tcpClient;
   TADOQuery *Query1;
   TADOConnection *MySQLCon;
   TJvVigenereCipher *jvCipher;
   TJvCaesarCipher *jvCipher1;
   TSpeedButton *btnUnlockS;
   TSpeedButton *btnLockS;
   TTabSheet *Convert;
   TButton *btnProcessC;
   TButton *btnCancelC;
   TSpeedButton *btnLockC;
   TSpeedButton *btnUnlockC;
   TLabel *Label9;
   TEdit *edtHostNameC;
   TLabel *Label10;
   TEdit *edtUserNameC;
   TLabel *Label11;
   TEdit *edtPasswordC;
   TLabel *Label12;
   TEdit *edtNewVersion;
   TStaticText *StaticText3;
   TLabel *Label13;
   TEdit *edtPrefixC;
   TLabel *Label15;
   TEdit *edtCurrVersion;
   TImage *Image1;
   TImage *Image4;
   TButton *btnGet;
   TStaticText *stProgress;
   TTabSheet *Upgrade;
   TButton *btnCancelU;
   TButton *btnUpgradeU;
   TStaticText *StaticText5;
   TImage *Image5;
   TListView *lvAlpha;
   TListView *lvNumeric;
   TRadioButton *rbSave;
   TRadioButton *rbRestore;
   TCheckBox *cbPersist;
   TCheckBox *cbIgnore;
   TCheckBox *cbDebug;
   TListView *lvExclude;
   TLabel *Label17;
   TLabel *Label19;
   TEdit *edtSubkey;
   TSpeedButton *SpeedButton4;
   TLabel *Label24;
   TEdit *edtRoot;
   TSpeedButton *btnLockU;
   TSpeedButton *btnUnlockU;
   TTabSheet *SQL;
   TStaticText *StaticText6;
   TLabel *Label25;
   TEdit *edtHostNameD;
   TSpeedButton *btnUnlockD;
   TButton *btnCancelD;
   TButton *btnProcessD;
   TImage *Image6;
   TLabel *Label26;
   TEdit *edtUserNameD;
   TLabel *Label27;
   TEdit *edtPasswordD;
   TLabel *Label28;
   TDataSource *dsSource;
   TSpeedButton *btnLockD;
   TLabel *Label29;
   TEdit *edtPrefixD;
   TComboBox *edtSQLD;
   TRadioButton *rbSilent;
   TLabel *Label30;
   TCheckBox *cbMultiCpy;
   TEdit *edtNewPrefixC;
   TLabel *Label31;
   TCheckBox *cbNewPass;
   TTabSheet *Restore;
   TStaticText *stMsgB;
   TLabel *Label32;
   TImage *Image7;
   TButton *btnCancelB;
   TButton *btnProcessB;
   TSpeedButton *btnLockB;
   TSpeedButton *btnUnlockB;
   TRadioButton *rbFull;
   TRadioButton *rbPartial;
   TCheckBox *cbType;
   TLabel *lblTitle;
   TLabel *lblDate;
   TLabel *lblMode;
   TEdit *edtTitle;
   TEdit *edtDate;
   TEdit *edtMode;
   TLabel *lblVersion;
   TLabel *lblTime;
   TLabel *lblType;
   TEdit *edtVersion;
   TEdit *edtTime;
   TEdit *edtType;
   TListView *lvTables;
   TSpeedButton *btnAllB;
   TStaticText *stProgressB;
   TTabSheet *Log;
   TButton *btnCancelL;
   TButton *btnProcessL;
   TSpeedButton *btnUnlockL;
   TSpeedButton *btnLockL;
   TImage *Image8;
   TStaticText *stMsgL;
   TLabel *Label76;
   TDateTimePicker *dtpSDate;
   TLabel *Label79;
   TDateTimePicker *dtpSTime;
   TLabel *Label33;
   TDateTimePicker *dtpEDate;
   TLabel *Label34;
   TDateTimePicker *dtpETime;
   TSpeedButton *btnReload;
   TLabel *Label56;
   TEdit *edtSearchUser;
   TSpeedButton *btnSearchUser;
   TLabel *Label57;
   TEdit *edtSearchDesc;
   TSpeedButton *btnSearchDesc;
   TSpeedButton *btnSearchBoth;
   TCheckBox *chkMatchAny;
   TLabel *Label35;
   TEdit *edtUser;
   TSpeedButton *btnFirst;
   TSpeedButton *btnPrev;
   TSpeedButton *btnNext;
   TSpeedButton *btnLast;
   TLabel *Label36;
   TEdit *edtDateL;
   TLabel *Label37;
   TEdit *edtTimeL;
   TLabel *Label38;
   TEdit *edtDescriptionL;
   TListView *lvLog;
   TStaticText *stMsg;
   TSpeedButton *btnRefresh;
   TSpeedButton *btnAll;
   TSpeedButton *btnArchive;
   TLabel *Label39;
   TButton *btnOpenLog;
   TCheckBox *chkAutoRefresh;
   TLabel *Label40;
   TEdit *edtUserL;
   TLabel *Label41;
   TEdit *edtPasswordL;
   TLabel *Label42;
   TEdit *edtHostL;
   TTimer *timTimer;
   TSaveDialog *sdArchive;
   TCheckBox *cbNewEncode;
   TAdvSpinEdit *speInterval;
   TAdvDirectoryEdit *edtFolder;
   TAdvFileNameEdit *edtSetupLoc;
   TAdvFileNameEdit *edtSQLFile;
   TAdvFileNameEdit *edtBackupFile;
   TSpeedButton *btnOpenB;
   TAdvFileNameEdit *edtArchive;
   TDBGrid *DBGrid1;
   TDBNavigator *DBNavigator1;
   TAdvEditBtn *edtKeyM;
   void __fastcall FormActivate(TObject *Sender);
   void __fastcall pgTabsChange(TObject *Sender);
   void __fastcall btnRegisterClick(TObject *Sender);
   void __fastcall btnCancelRClick(TObject *Sender);
   void __fastcall btnProcessFClick(TObject *Sender);
   void __fastcall edtPrefixMChange(TObject *Sender);
   void __fastcall btnProcessMClick(TObject *Sender);
   void __fastcall edtNameChange(TObject *Sender);
   void __fastcall edtHostNameChange(TObject *Sender);
   void __fastcall btnUnlockSClick(TObject *Sender);
   void __fastcall btnLockCClick(TObject *Sender);
   void __fastcall btnProcessCClick(TObject *Sender);
   void __fastcall edtHostNameCChange(TObject *Sender);
   void __fastcall btnGetClick(TObject *Sender);
   void __fastcall btnUpgradeUClick(TObject *Sender);
   void __fastcall btnLockUClick(TObject *Sender);
   void __fastcall rbSaveClick(TObject *Sender);
   void __fastcall rbRestoreClick(TObject *Sender);
   void __fastcall btnLockDClick(TObject *Sender);
   void __fastcall btnProcessDClick(TObject *Sender);
   void __fastcall edtHostNameDChange(TObject *Sender);
   void __fastcall rbSilentClick(TObject *Sender);
   void __fastcall btnProcessBClick(TObject *Sender);
   void __fastcall btnUnlockBClick(TObject *Sender);
   void __fastcall rbFullClick(TObject *Sender);
   void __fastcall rbPartialClick(TObject *Sender);
   void __fastcall btnAllBClick(TObject *Sender);
   void __fastcall lvTablesItemChecked(TObject *Sender, TListItem *Item);
   void __fastcall edtKeyMChange(TObject *Sender);
   void __fastcall FormDestroy(TObject *Sender);
   void __fastcall btnProcessLClick(TObject *Sender);
   void __fastcall btnLockLClick(TObject *Sender);
   void __fastcall edtUserLChange(TObject *Sender);
   void __fastcall btnOpenLogClick(TObject *Sender);
   void __fastcall edtArchiveChange(TObject *Sender);
   void __fastcall btnFirstClick(TObject *Sender);
   void __fastcall btnPrevClick(TObject *Sender);
   void __fastcall btnNextClick(TObject *Sender);
   void __fastcall btnLastClick(TObject *Sender);
   void __fastcall MoveItems(void);
   void __fastcall timTimerTimer(TObject *Sender);
   void __fastcall chkAutoRefreshClick(TObject *Sender);
   void __fastcall speIntervalChange(TObject *Sender);
   void __fastcall btnReloadClick(TObject *Sender);
   void __fastcall btnAllClick(TObject *Sender);
   void __fastcall btnArchiveClick(TObject *Sender);
   void __fastcall lvLogClick(TObject *Sender);
   void __fastcall btnSearchUserClick(TObject *Sender);
   void __fastcall btnSearchDescClick(TObject *Sender);
   void __fastcall btnSearchBothClick(TObject *Sender);
   void __fastcall edtFolderDialogClose(TObject *Sender, UnicodeString &NewDirectory, bool OK);
   void __fastcall edtSetupLocClickBtn(TObject *Sender);
   void __fastcall edtSQLFileClickBtn(TObject *Sender);
   void __fastcall edtBackupFileClickBtn(TObject *Sender);
   void __fastcall edtBackupFileDialogExit(TObject *Sender, bool ExitOK);
   void __fastcall edtArchiveClickBtn(TObject *Sender);
   void __fastcall edtArchiveDialogExit(TObject *Sender, bool ExitOK);
   void __fastcall ColSizes(TObject *Sender);
   void __fastcall Query1AfterOpen(TDataSet *DataSet);
   void __fastcall edtKeyMEnter(TObject *Sender);
   void __fastcall edtKeyMClickBtn(TObject *Sender);


private:	// User declarations
   bool __fastcall  DM_Connect_DB(void);
   bool __fastcall  DM_Open_Connection(void);
   bool __fastcall  DM_Put_DB(AnsiString S1, int RunType);
   bool __fastcall  DM_PutSignature(void);
   bool __fastcall  SetDateTimeDef(int Type);
   bool __fastcall  ReadAllRecs(AnsiString UserStr, AnsiString DescStr, AnsiString S2);
   bool __fastcall  ReadAllLogRecs(AnsiString UserStr, AnsiString DescStr, AnsiString S2);
   bool __fastcall  DeleteItem(int Key);
   void __fastcall  ProcessResponse(AnsiString Response);
   int  __fastcall  ldOpt(int argc, char *argv[], AnsiString OptList, TStringList *Options, TStringList *Parms);
   void __fastcall  SilentUpgrade(void);
   void __fastcall  DoNewEncoding(void);
   void __fastcall  DoNewStylePass(void);
   void __fastcall  DoMultiCompany(void);
   int  __fastcall  FindTable(AnsiString ThisTable, int idx1);
   void __fastcall  GetList(AnsiString FileName);
   void __fastcall  ColSizes(TDBGrid *DBGrid);
   AnsiString __fastcall InputQueryM(AnsiString Caption, AnsiString Question, int DispType);
   AnsiString __fastcall EncodePass(AnsiString ThisPass);
   AnsiString __fastcall ReplaceQuote(AnsiString S1, int Type);
   AnsiString __fastcall Assemble(TStringList *List);
   TStringList          *Disassemble(char *Str);
   AnsiString __fastcall ReplaceXML(AnsiString S1, int Type);

   TStringList *Disassemble(char *Str, int Type);

   int  CallHWND;
   bool LockS_State, LockC_State, LockU_State, LockD_State, LockB_State;
   bool LockL_State, Selected, SearchBoth, SearchUser, SearchDesc, Sema;
   bool DateIsSet, ArchiveActive, SemaSQL;

   AnsiString SQLFile, HostName, UserName, Password, Prefix, FilesDir;
   AnsiString UserKey, ClientName, ClientEmail, ClientUnique;
   AnsiString ClientContact, ServerName, ServerPort, LPMSUpgrade;
   AnsiString ThisGUID, ThisInstall, KeepRegString, RestoreTables;
   AnsiString CSMySQL;              // Connection string parameters
   AnsiString ErrMsg;               // Last error message

   TStringList *RespList, *ImportList, *TableList;
   TStringList *LogList;

   _di_IXMLNode ExportSet, TableSet, ThisNode;

//--- Used for trapping the BackSpace Key

   bool Registered, BackSpace;
   MESSAGE void __fastcall WMHotKey(TWMHotKey &Message);

public:		// User declarations
   AnsiString __fastcall Vignere(int Type, char Phrase[], char Key[]);

   bool       MultiCompany, Proceed, DoRestore;
   AnsiString DBPrefix, ThisPass, ThisDBPrefix, RestoreHost, RestoreUser;
   AnsiString RestorePass, Result;

//--- Used for trapping the BackSpace Key

BEGIN_MESSAGE_MAP
VCL_MESSAGE_HANDLER(WM_HOTKEY, TWMHotKey, WMHotKey)
END_MESSAGE_MAP(TForm)

   __fastcall TFLPMS_FirstRunApp(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFLPMS_FirstRunApp *FLPMS_FirstRunApp;
//---------------------------------------------------------------------------
#endif
