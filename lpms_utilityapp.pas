unit LPMS_UtilityApp;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
   ExtCtrls, Buttons, DBGrids, DBCtrls, EditBtn, Spin, DateTimePicker;

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
      SQL: TTabSheet;
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
      procedure Label43Click(Sender: TObject);
   private

   public

   end;

var
   FLPMS_UtilityApp: TFLPMS_UtilityApp;

implementation

{$R *.lfm}

{ TFLPMS_UtilityApp }

procedure TFLPMS_UtilityApp.Label43Click(Sender: TObject);
begin

end;

end.

