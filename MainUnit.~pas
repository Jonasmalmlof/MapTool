//------------------------------------------------------------------------------
//
// The Main Form
//
// Cre 2004-02-17 Pma
//
//------------------------------------------------------------------------------
unit MainUnit;

interface

uses
  // Default by Delpi

  Controls, Dialogs, Menus, ExtCtrls, Classes, ActnList, ComCtrls, StdCtrls,

  // My additions

  Windows,  // TPoint
  Forms,    // TForm
  SysUtils, // FIle Name utils
  Jpeg,     // JPEG Image
  Math,     // Min, Max etc
  clipbrd,  // Clipboard
  IniFiles, // Ini Files
  StrUtils, // Ansi strings
  Buttons,  // Buttons
  ToolWin,  // Toolbar
  ImgList,  // Image List
  Graphics, // clWhite

  // My very own

  MapImage,         // Map Image
  MapItem,          // Map Item Object
  MapItemList,      // Map Item List
  MapItemType,      // Map Item Type
  MapItemTypeList,  // Map Item Type list
  LeafUnit,         // Save/Loading
  GenPref,          // Prefereses dialog
  AboutBox,         // The About Form
  DirDialog,
  GeomUtils;        // Geometrical utilities

const

  // Save/Load (DONT CHANGE NUMBERS, ADD TO BOTTOM)

  LeafMapInfo      = 0;
  LeafMapName      = 1;
  LeafItemTypeList = 2;
  LeafItemList     = 3;

  // Status row panels index

  StatusApp   = 0;
  StatusMap   = 1;
  StatusItem  = 2;
  StatusScale = 3;
  StatusPos   = 4;

  // Action Icons index in Toolbar

  IconExit  = 0;
  IconSave  = 1;
  IconPref  = 2;
  IconPoint = 3;
  IconScale = 4;
  IconEdit  = 5;
  IconFont  = 6;
  IconDir   = 7;
  IconHint  = 8;
  IconAbout = 9;

  MapItemLastMin = 0;
  MapItemLastMax = 30;

  MapDirParent  = '.. (Parent)';
  MapDirDesktop = '.. (Desktop)';

type TAppStatus = (asOnOpening, asOnRunning, asOnClosing);

type
  TTheMainForm = class(TForm)
    ActionList1: TActionList;
    ActionExit: TAction;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    ActionSetFont: TAction;
    FontDialog: TFontDialog;
    View1: TMenuItem;
    Font1: TMenuItem;
    SetHintsOnOff: TMenuItem;
    ActionSetHintOnOff: TAction;
    BasePanel: TPanel;
    ListPanel: TPanel;
    MapSplitter: TSplitter;
    MapListBox: TListView;
    ListSplitter: TSplitter;
    MapItemListBox: TListView;
    DescSplitter: TSplitter;
    CursorTimer: TTimer;
    SaveMap: TMenuItem;
    ActionSaveMap: TAction;
    ListItemPopup: TPopupMenu;
    MenueNewItem: TMenuItem;
    MenuDeleteItem: TMenuItem;
    DrawAll: TMenuItem;
    DrawNone: TMenuItem;
    MapPanel: TPanel;
    ScrlRightDown: TBitBtn;
    ScrlDown: TBitBtn;
    ScrlLeftDown: TBitBtn;
    ScrlLeft: TBitBtn;
    ScrlLeftUp: TBitBtn;
    ScrlUp: TBitBtn;
    ScrlRightUp: TBitBtn;
    ScrlRight: TBitBtn;
    Map: TImage;
    MapTimer: TTimer;
    SetMapDir: TMenuItem;
    ActionSetMapDir: TAction;
    SetMapDirDialog: TOpenDialog;
    Preferenses1: TMenuItem;
    Help1: TMenuItem;
    ShowAboutBox: TMenuItem;
    ActionShowAbout: TAction;
    ScaleIncrement40: TMenuItem;
    ScaleIncrement20: TMenuItem;
    ScaleIncrement10: TMenuItem;
    StatusBar: TStatusBar;
    MapItemDesc: TMemo;
    ToolBarImages: TImageList;
    ActionScaleFull: TAction;
    ActionScale100: TAction;
    SetItemTypeSub: TMenuItem;
    MapItemTimer: TTimer;
    MenuSetItemInMiddle: TMenuItem;
    ActionScale20: TAction;
    ActionScale40: TAction;
    ActionScale60: TAction;
    ActionScale80: TAction;
    ActionScale120: TAction;
    ActionScaleUp: TAction;
    ActionScaleDown: TAction;
    ActionScaleInc10: TAction;
    ActionScaleInc20: TAction;
    ActionScaleInc40: TAction;
    ActionSetEditMode: TAction;
    ActionItemNew: TAction;
    ActionItemDel: TAction;
    ActionItemSetInMid: TAction;
    MenuEdit: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuSetInMiddle: TMenuItem;
    MenuDeleteItem1: TMenuItem;
    MenuEditMode1: TMenuItem;
    N1: TMenuItem;
    N3: TMenuItem;
    EditMode1: TMenuItem;
    SetNewItemType: TMenuItem;
    ActionSetNewItemType: TAction;
    ActionShowAllTypes: TAction;
    ActionShowNoTypes: TAction;
    ToolbarToggle: TMenuItem;
    ActionToolbarToggle: TAction;
    ActionItemCopy: TAction;
    ActionItemPaste: TAction;
    opyItem1: TMenuItem;
    Paste1: TMenuItem;
    N4: TMenuItem;
    CopyItem1: TMenuItem;
    PasteItem1: TMenuItem;
    ActionUndo: TAction;
    Undo1: TMenuItem;
    Undo2: TMenuItem;
    ActionShowNifty: TAction;
    DrawTimer: TTimer;
    HighlightItem1: TMenuItem;
    ActionShowAllItems: TAction;
    HighlighAll1: TMenuItem;
    ToolbarButtons: TMenuItem;
    N5: TMenuItem;
    MenuItemType: TMenuItem;
    ActionSetNewItemType1: TMenuItem;
    NewTypeSpace: TMenuItem;
    Scale1: TMenuItem;
    ScaleFull1: TMenuItem;
    ScaleDown1: TMenuItem;
    Scale201: TMenuItem;
    Scale401: TMenuItem;
    Scale601: TMenuItem;
    Scale801: TMenuItem;
    Scale1001: TMenuItem;
    Scale1201: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    ScaleUp1: TMenuItem;
    ActionSetItemColor: TAction;
    ItemColorDlg: TColorDialog;
    SetItemColor1: TMenuItem;
    SetItemColor2: TMenuItem;
    SelectMapTimer: TTimer;
    ActionSetFontName: TAction;
    ActionSetFontHeight: TAction;
    CoolBar: TCoolBar;
    ToolBarFile: TToolBar;
    ToolBarEdit: TToolBar;
    ToolBarView: TToolBar;
    ToolBarScale: TToolBar;
    ToolBarFont: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonUndo: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    FontCombo: TComboBox;
    FontComboHeight: TComboBox;
    Preferenses2: TMenuItem;
    ActionPref: TAction;
    ActionOpenMap: TAction;
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionSetFontExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionSetHintOnOffExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MapSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure MapSplitterMoved(Sender: TObject);
    procedure MapItemListBoxColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure MapItemListBoxCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure MapListBoxColumnClick(Sender: TObject; Column: TListColumn);
    procedure MapListBoxCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ListSplitterMoved(Sender: TObject);
    procedure DescSplitterMoved(Sender: TObject);
    procedure DescSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure CursorTimerTimer(Sender: TObject);
    procedure ActionSaveMapExecute(Sender: TObject);
    procedure ScrlRightClick(Sender: TObject);
    procedure ScrlLeftClick(Sender: TObject);
    procedure ScrlUpClick(Sender: TObject);
    procedure ScrlDownClick(Sender: TObject);
    procedure ScrlLeftUpClick(Sender: TObject);
    procedure ScrlLeftDownClick(Sender: TObject);
    procedure ScrlRightDownClick(Sender: TObject);
    procedure ScrlRightUpClick(Sender: TObject);
    procedure MapTimerTimer(Sender: TObject);
    procedure ActionSetMapDirExecute(Sender: TObject);
    procedure ActionShowAboutExecute(Sender: TObject);
    procedure ActionScaleFullExecute(Sender: TObject);
    procedure ActionScale100Execute(Sender: TObject);
    procedure MapItemListBoxEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MapItemListBoxSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MapListBoxInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure MapItemListBoxInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure MapItemTimerTimer(Sender: TObject);
    procedure ActionScale20Execute(Sender: TObject);
    procedure ActionScale40Execute(Sender: TObject);
    procedure ActionScale60Execute(Sender: TObject);
    procedure ActionScale80Execute(Sender: TObject);
    procedure ActionScale120Execute(Sender: TObject);
    procedure ActionScaleDownExecute(Sender: TObject);
    procedure ActionScaleUpExecute(Sender: TObject);
    procedure ActionScaleInc10Execute(Sender: TObject);
    procedure ActionScaleInc20Execute(Sender: TObject);
    procedure ActionScaleInc40Execute(Sender: TObject);
    procedure ActionSetEditModeExecute(Sender: TObject);
    procedure ActionItemNewExecute(Sender: TObject);
    procedure ActionItemDelExecute(Sender: TObject);
    procedure ActionItemSetInMidExecute(Sender: TObject);
    procedure ActionSetNewItemTypeExecute(Sender: TObject);
    procedure ActionShowNoTypesExecute(Sender: TObject);
    procedure ActionShowAllTypesExecute(Sender: TObject);
    procedure ActionToolbarToggleExecute(Sender: TObject);
    procedure ActionItemCopyExecute(Sender: TObject);
    procedure ActionItemPasteExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionShowNiftyExecute(Sender: TObject);
    procedure DrawTimerTimer(Sender: TObject);
    procedure ActionShowAllItemsExecute(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure ActionSetItemColorExecute(Sender: TObject);
    procedure SelectMapTimerTimer(Sender: TObject);
    procedure MapItemListBoxEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure MapListBoxClick(Sender: TObject);
    procedure FontComboMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ActionSetFontNameExecute(Sender: TObject);
    procedure ActionSetFontHeightExecute(Sender: TObject);
    procedure CoolBarResize(Sender: TObject);
    procedure CoolBarDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ToolBarScaleEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure ToolbarButtonsClick(Sender: TObject);
    procedure ToolBarFontEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure ToolBarViewEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure ToolBarEditEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure ToolBarFileEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure ToolBarScaleClick(Sender: TObject);
    procedure ActionPrefExecute(Sender: TObject);
    procedure ActionOpenMapExecute(Sender: TObject);
  private
    { Private declarations }

    // Load all maps into the Map Listbox using the current MapDirectory

    function  MapSetDirDialog : boolean;    // Set new Map directory
    procedure MapListBoxEmpty;        // Clear all maps loaded
    procedure MapListBoxLoad;         // Load list box from current dir
    procedure MapListBoxLoadThis (fileName : string);
    procedure MapListBoxSetWdt;       // Adjust wdt of columns
    procedure MapListBoxSelectCurMap; // Select cur map in listbox

    // Load a Map with MapName

    function  MapDirSet (inDir : string): boolean;  // Set new directory
    procedure MapSaveOld;                           // Save old if user wants to
    procedure MapLoad (inMapName : string);         // Load a map
    procedure MapSaveToFile;                        // Save map items
    procedure MapLoadFromFile (sMapName : string);  // Load map items

    // Handle Item in form

    procedure MapItemSelect (mp : TMapItem);

    //--- Handel MapItemListBox ------------------------------------------------

    procedure MapItemListBoxLoad;                    // Load listbox with items
    procedure MapItemListBoxAdd (mp : TMapItem);     // Add one item to listbox
    procedure MapItemListBoxDel (mp : TMapItem);     // Remove one item
    procedure MapItemListBoxSel (mp : TMapItem);     // Select Item
    function  MapItemListBoxInqItem : TMapItem;      // return item selcted
    procedure MapItemListBoxSetName (mp : TMapItem); // Set new name on an item
    procedure MapItemListBoxSetWdt;                   // Adjust wdt of columns

    // Save and Load all INI settings

    procedure IniSave;
    procedure IniLoad;
    procedure IniSaveLastItemsAdd(it : TMapItemType);

    procedure Scale (dir : TDirections); overload ;
    procedure Scale (scl : real; pScr : TPoint); overload ;
    procedure Scale (scl : real); overload ;
                         
    procedure DrawNifty (mi : TMapItem);

    //--- Mouse Handling depending on Edit Mode --------------------------------

    // Handle scrolling of the map on Mouse Move event

    function  MapMouseScrollMove (Shift: TShiftState; X, Y: Integer):boolean;

    function MapMouseInqScrollStopped : boolean;

    function MapMouseItemMoving(Shift: TShiftState; X,Y: Integer) : boolean;

    // Test if mouse is moving over a Map Item (if, change cursor only)

    function MapMouseTestItem (Shift: TShiftState; X, Y: Integer) : boolean ;

    // Test if mouse can select an item (if: show it)

    function MapMouseSelectItem (Shift: TShiftState; X, Y: Integer) : boolean ;

    // Scale the map from this position

    procedure MapMouseScaleMap(Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);

    // Test if user moves over an selected items edit frame

    function MapMouseTestHitItem(Shift: TShiftState; X,Y: Integer):boolean;

    function MapMouseItemChanged (Shift: TShiftState): boolean;

    // EditMode = mdModePoint

    procedure MapMousePointMove (Sender: TObject;
                                Shift: TShiftState; X, Y: Integer);

    // EditMode = mdModeEdit

    procedure MapMouseEditMove (Sender: TObject;
                                Shift: TShiftState; X, Y: Integer);

    // Update the description field

    procedure SetDesc (desc : string);

    function InqMapMid : TPoint; // Return middle of the map position

    procedure SetItemType(it : TMapItemType);

    //--- GUI update procedure -------------------------------------------------
    //    Does not do anything but update Gui to reflect changes

    
    // Calculate all controls positions
    //    iType = 0 Just adjust all controls as they are (redraw)
    //    iType = 1 Make sure aspects ratios are kept (resize mainform)
    //    iType = 2 Make sure MapPanel are splittered (move splitter)
    //    iType = 3 Make sure ListPanel are splittered (move splitter)
    //    iType = 4 Make sure DescPanel are splittered (move splitter)

    procedure GuiUpdateControls (iType : integer);

    // Handle Item Visability

    procedure SetItemTypeVisibility(Sender: TObject);
    procedure SetItemTypeFromMenu(Sender: TObject);
    procedure ClearAllItemTypesInMenus;
    procedure GuiUpdateMapItemTypes;

    // Update Gui to reflect the state of things

    procedure GuiUpdateMapScroll;     // Update all scroll buttons and menues
    procedure GuiUpdateMapScale;      // Update all scale buttons and menues
    procedure GuiUpdateItemSelection; // Update all about selection of items

    // Set ShowHint on and off

    procedure SetHint (bHint : boolean);  // Set the Hint on or off

    procedure MyShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);

    // Font Manipulations

    procedure FontInitCombo;
    procedure FontSetCombo;
    procedure FontIniSave (var Ini : TIniFile);
    procedure FontIniLoad (var Ini : TIniFile);

    // CoolBar Manipulations

    Procedure CoolBarIniSave (var Ini : TIniFile);
    Procedure CoolBarIniLoad (var Ini : TIniFile);
    procedure CoolBarInitGui;
    procedure CoolBarMenuCallBack (Sender: TObject);

    // Preferenses

    procedure PrefInit;
    procedure PrefIniSave (var Ini : TIniFile);
    procedure PrefIniLoad (var Ini : TIniFile);

  public
    { Public declarations }

    // Nothing is needed here
  end;

var
  TheMainForm: TTheMainForm;

  AppStatus : TAppStatus = asOnOpening;

  //----------------------------------------------------------------------------
  // Save / Load Item Types
  //
  LeafsMap : TLeafRecordArray;

  //----------------------------------------------------------------------------
  //                          User Interface globals
  //----------------------------------------------------------------------------
  // Hint on controls

  bHintOn : boolean = false;

  DrawNiftyOn     : boolean = true;
  DrawNiftyIndex  : integer;
  DrawNiftyFirst  : integer;
  DrawNiftyLast   : integer;
  DrawNiftyItem   : TMapItem;
  DrawNiftyRand   : integer;
  DrawNiftyFrom   : boolean;

  DrawNiftyRec : TNiftyRec;

  HighlightAllItems : boolean = false;

  AboutDlg       : TAboutForm = nil; // The About dialog
  AboutDlgRect   : TRect;            // Place of the About Dialog
  AboutDlgActive : integer;          // Active Tab set

  // mdPoint = read only, mdEdit = Write allowed

  EditMode          : boolean = false; // Current edit mode
  EditModeLast      : boolean = true;  // Last edit mode in Update Gui
  EditModeLastItem  : boolean = false; // Last Edit item in Update Gui

  // Mouse things

  MouseDownLastPos : TPoint; // Position for last mouse down event

  // sorting of listboxes

  ColumnToSortMaps          : Integer = 0;
  ColumnToSortMapsNameDown  : boolean = true;
  ColumnToSortMapsDescDown  : boolean = true;

  ColumnToSortItems         : Integer = 0;
  ColumnToSortItemsNameDown : boolean = true;
  ColumnToSortItemsTypeDown : boolean = true;

  // Splitter / Resize

  globSplitterDescHeight : integer = 100; // Height of DescPanel
  globSplitterListHeight : integer = 100; // Height of MapListbox
  globSplitterListWidth  : integer = 100; // Width  of Listboxes
  globMainFormPainting : boolean = true;  // true if painting is allowed

  //----------------------------------------------------------------------------
  //                              Map globals
  //----------------------------------------------------------------------------

  MapName      : string = '';     // Name of current loaded map (without ext)
  MapDir       : string = '';     // Current directory for maps
  MapExt       : string = '';     // Extent used for current Map

  MapImageObj : TMapImage = nil; // The object handling the Map

  MapItemNameEditing : boolean = false; // true if an item name is open for edit

  // About map scrolling / moving

  MapMoveStarted : boolean = false; // True is map is moved by the mouse
  MapMoveStartPos : TPoint;         // Start position for this move

  // All about Map points and such

  MapItemList     : TMapItemList = nil;        // Map Data for current Map
  MapItemCur      : TMapItem = nil;       // Current selected item
  MapPointSelItem : TMapItemSelection = msNone; // Current selection inside item
  MapItemCopy     : TMapItem = nil;      // Pointer to copied item

  // All about Map Item Types list

  MapItemTypeList : TMapItemTypeList = nil; // GLobal map item types in a list

  // Genereal move, scale, rot

  MapItemMoveStarted : boolean = false;    // Item move has started
  MapItemMoveType : TMapItemSelection = msNone; // Selected part of selected item
  MapItemMovePointIndex : integer;         // index of point moving
  MapItemMovePointFirst : boolean = false; // First move of a point
  MapItemMoveItemExt  : TRect;             // Item extent that are moving
  MapItemMoveMidPoint : TPoint;            // Mid point of item

  // Move item

  MapItemMoveStartPos : TPoint; // Start position in map coords
  MapItemMoveLastDist : TPoint; // Last position of an item move

  // Rotate item

  MapItemMoveStartAng : real;   // Start angle of an rotation
  MapItemMoveLastAng  : real;   // Last angle of an rotation

  // Scale item

  MapItemScaleStartPos : TPoint; // Start distance position for scaling
  MapItemScaleLastSclX : real;   // Last scale in X direction
  MapItemScaleLastSclY : real;   // Last Scale in Y direction

  // Item types last used

  MapItemTypesLastUsed : array [MapItemLastMin..MapItemLastMax] of TMapItemIni;

  MapResizeFirst : boolean = true;
  MapResizeMapPos : TPoint;

implementation

{$R *.dfm}

uses
  MapItemTypeNew,   // New Item Type dialog
  MapUtils,         // General Map utilities
  GenUtils;         // My general utilities

//------------------------------------------------------------------------------
//                               Initialization
//------------------------------------------------------------------------------
// Create the form and initialize all data
//
procedure TTheMainForm.FormCreate(Sender: TObject);
begin

  AppStatus := asOnOpening;
  Randomize;

  LoadCursors; // Load all cursors used in map

  MapImageObj     := TMapImage.Create(Map, StatusBar);
  MapItemList     := TMapItemList.Create;
  MapItemTypeList := TMapItemTypeList.Create;

  // Set some default that will make things work witout an Ini file

  MapDir := ExtractFilePath(Application.ExeName);
  MapName := '';

  // Initiate the Draw Nifty record

  PrefInit;

  Application.OnShowHint := MyShowHint;

  // Initialize the Font Combo boxes

  FontInitCombo;

  AboutDlg := TAboutForm.Create(self);

  MapResizeFirst := false;
  GuiUpdateControls (0);  // Make sure all is drawn in the right size
  CoolBarInitGui;

  IniLoad;        // Read all things from Ini file

  // Update all things in GUI

  GuiUpdateMapScroll;
  GuiUpdateMapScale;
  GuiUpdateMapItemTypes;
  GuiUpdateItemSelection;

  AppStatus := asOnRunning;
end;
//------------------------------------------------------------------------------
//  User is closing the application
//
procedure TTheMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  r : integer;
  s : string;
begin
  CanClose := true;

  // Test Map item changes

  if MapItemList <> nil then
    begin
      // Shut down selected

      if MapItemList.InqDirty or MapItemNameEditing then
        begin
          s :=  'You have changed ' + MapName +
                  '. Do you want to save it before closing (answer Yes)?' +
                  ' Or do you want to cancel closing (answer Cancel)?';
          r := Application.MessageBox(PAnsiChar(s), 'Exit',MB_YESNOCANCEL	);
          if (r = 0) or (r = IDCANCEL) then
            CanClose := false
          else if (r = IDYES) then
            MapSaveToFile;
        end
    end
end;
//------------------------------------------------------------------------------
// User closed the application
//
procedure TTheMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AppStatus := asOnClosing;

  // Turn off any timers

  DrawTimer.Enabled      := false;
  MapTimer.Enabled       := false;
  MapItemTimer.Enabled   := false;
  CursorTimer.Enabled    := false;
  SelectMapTimer.Enabled := false;

  // Reset all selections

  MapItemSelect (nil);

  // Save all ini settings

  IniSave;

  // Reset all other things

  MapListBoxEmpty;

  // Free objects created

  MapItemList.Free;
  MapItemList := nil;

  MapItemTypeList.ClearAll; // Don't call free

  MapImageObj.Free;
  MapImageObj := nil;
end;
//------------------------------------------------------------------------------
//                              INI-File functions
//------------------------------------------------------------------------------
// Load all settings from INI-File
//
procedure TTheMainForm.IniLoad;
var
  Ini         : TIniFile;  // .ini file
  sMapName    : string;
  l,w,t,h  : integer;
  MapScale    : real;
begin

  // Read all things from Ini file

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try

    // Map things

    MapDir := Ini.ReadString( 'Map', 'MapDir', MapDir);
    sMapName := Ini.ReadString( 'Map', 'MapName', 'Numenor');
    l := Ini.ReadInteger( 'Map', 'ScaleInc', 100);
    case l of
    10: MapImageObj.SetScaleIncrement(10);
    20: MapImageObj.SetScaleIncrement(20);
    else
        MapImageObj.SetScaleIncrement(40);
    end;

    MapScale := Ini.ReadInteger( 'Map', 'Scale', 1) / 10;
    MapResizeMapPos.X := Ini.ReadInteger( 'Map', 'PosX', 0);
    MapResizeMapPos.Y := Ini.ReadInteger( 'Map', 'PosY', 0);

    // Load Font Settings

    FontIniLoad (Ini);

    // Load CollBar setting

    CoolBarIniLoad(Ini);

    // Load Preferenses

    PrefIniLoad (Ini);

    SetHint (Ini.ReadBool( 'Misc', 'Hint', false));

    DrawNiftyOn       := Ini.ReadBool( 'Misc', 'Highlight', false);
    HighlightAllItems := Ini.ReadBool( 'Misc', 'ShowAllItems', false);

    // About dialog

    AboutDlgRect.Left := Ini.ReadInteger('About','Left',20);
    AboutDlgRect.Right := Ini.ReadInteger('About','Width',AboutDlg.Width);
    AboutDlgRect.Top := Ini.ReadInteger('About','Top',100);
    AboutDlgRect.Bottom := Ini.ReadInteger('About','Height',AboutDlg.Height);
    AboutDlgActive := Ini.ReadInteger('About','Active',0);

    if Ini.ReadBool('About','On', false) then
      ActionShowAboutExecute (self);

    // Splitter Bars

    l := Ini.ReadInteger( 'Splitter', 'Map', 0);
    if l > 0 then
      globSplitterListWidth := l;

    l := Ini.ReadInteger( 'Splitter', 'List', 0);
    if l > 0 then
      globSplitterListHeight := l;

    l := Ini.ReadInteger( 'Splitter', 'Desc', 0);
    if l > 0 then
      globSplitterDescHeight := l;

    // Save the latest used items

    for l := MapItemLastMin to MapItemLastMax do
      begin
        MapItemTypesLastUsed[l].TypeName :=
                  Ini.ReadString( 'MapItems', 'LastName' + IntToStr(l), '');

        MapItemTypesLastUsed[l].TypeGeom :=
                  Ini.ReadString( 'MapItems', 'LastGeom' + IntToStr(l), '');
      end;

    // Restore the main window in right place and style
    
    l := Ini.ReadInteger('MainForm','Left',Left);
    w := Ini.ReadInteger('MainForm','Width',Width);
    t := Ini.ReadInteger('MainForm','Top',Top);
    h := Ini.ReadInteger('MainForm','Height',Height);

    SetBounds(l,t,w,h);

    case Ini.ReadInteger('MainForm','State',1) of
      1: self.WindowState := wsNormal;
      2: self.WindowState := wsMinimized;
      3: self.WindowState := wsMaximized;
    end;

  finally
    Ini.Free;
  end;

  // Load maps in directory

  MapListBoxLoad;

  // Load Map

  if length(sMapName) = 0 then
    MapLoad ('Numenor.jpg')
  else
    MapLoad(sMapName);

  // Set scale and position in map

  MapImageObj.Scale(MapScale);
end;
//------------------------------------------------------------------------------
// Save all settings to INI-File
//
procedure TTheMainForm.IniSave;
var
  Ini : TIniFile;  // .ini file
  i   : integer;
begin

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try

    // Save Font Settings

    FontIniSave (Ini);

    // Save CoolBar settings

    CoolBarIniSave(Ini);

    // Save Preferenses

    PrefIniSave (ini);

    Ini.WriteBool   ('Misc', 'Hint', bHintOn);
    Ini.WriteBool   ('Misc', 'ToolBar', ActionToolBarToggle.Checked);

    Ini.WriteBool( 'Misc', 'Highlight', DrawNiftyOn);
    Ini.WriteBool( 'Misc', 'ShowAllItems', HighlightAllItems);

    // Map things

    Ini.WriteString('Map', 'MapName', MapName + MapExt);
    Ini.WriteString('Map', 'MapDir', MapDir);
    Ini.WriteInteger( 'Map', 'ScaleInc', MapImageObj.InqScaleIncrement());
    Ini.WriteInteger( 'Map', 'Scale', round(MapImageObj.InqScale() * 10));
    Ini.WriteInteger( 'Map', 'PosX', MapImageObj.InqMapPntCenScr().X);
    Ini.WriteInteger( 'Map', 'PosY', MapImageObj.InqMapPntCenScr().Y);

    // Splitter bars

    Ini.WriteInteger( 'Splitter', 'Map',  globSplitterListWidth);
    Ini.WriteInteger( 'Splitter', 'List', globSplitterListHeight);
    Ini.WriteInteger( 'Splitter', 'Desc', globSplitterDescHeight);

    // Tell if About is open and the place of it

    Ini.WriteBool   ('About','On', AboutDlg.Visible );
    Ini.WriteInteger('About','Left',AboutDlg.Left);
    Ini.WriteInteger('About','Width',AboutDlg.Width);
    Ini.WriteInteger('About','Top',AboutDlg.Top);
    Ini.WriteInteger('About','Height',AboutDlg.Height);
    Ini.WriteInteger('About','Active',AboutDlg.TabViews.ActivePageIndex);

    // Tell about the main window

    Ini.WriteInteger('MainForm','Left',Left);
    Ini.WriteInteger('MainForm','Width',Width);
    Ini.WriteInteger('MainForm','Top',Top);
    Ini.WriteInteger('MainForm','Height',Height);

    case WindowState of
      wsNormal    : Ini.WriteInteger('MainForm','State',1);
      wsMinimized : Ini.WriteInteger('MainForm','State',2);
      wsMaximized : Ini.WriteInteger('MainForm','State',3);
    end;

    // Save the latest used items

    for i := MapItemLastMin to MapItemLastMax do
      begin
        if length(MapItemTypesLastUsed[i].TypeName) > 0 then
          begin
            Ini.WriteString( 'MapItems', 'LastName' + IntToStr(i),
                              MapItemTypesLastUsed[i].TypeName);
            Ini.WriteString( 'MapItems', 'LastGeom' + IntToStr(i),
                              MapItemTypesLastUsed[i].TypeGeom);
          end;
      end;

  finally
    Ini.Free;
  end;

end;
//------------------------------------------------------------------------------
// Add a new type used
//
procedure TTheMainForm.IniSaveLastItemsAdd(it : TMapItemType);
var
  i  : integer;
begin
  if it <> nil then
    begin

      // Test if it exist already

      for i := MapItemLastMin to MapItemLastMax do
        if CompareStr(MapItemTypesLastUsed[i].TypeName, it.InqName()) = 0 then
          exit;

      // Move all items upp one step

      for i := MapItemLastMax downto MapItemLastMin + 1 do
        MapItemTypesLastUsed[i] := MapItemTypesLastUsed[i - 1];

      // Add the new item first

      MapItemTypesLastUsed[MapItemLastMin].TypeName := it.InqName();
      MapItemTypesLastUsed[MapItemLastMin].TypeGeom := it.InqGeometryName();
    end;

end;
//------------------------------------------------------------------------------
//                        Loading Listboxes and Maps
//------------------------------------------------------------------------------
//  Set new Map directory
//
function TTheMainForm.MapSetDirDialog : boolean;
var
  sTemp, sExt, sName  : string;
begin
  MapSetDirDialog := false;

  // Start where we are now

  SetMapDirDialog.InitialDir := MapDir;

  // Set the right filter

  SetMapDirDialog.Filter :=
      'All Possible Pictures |*.JPG;*.JPEG;*.BMP|' +
      'Jpeg Pictures (*.jpg)|*.JPG;*.JPEG|' +
      'Windows Bitmaps (*.bmp)|*.BMP|';

  // Open the dialog

  if SetMapDirDialog.Execute then
    begin
      sTemp := SetMapDirDialog.FileName;

      // Did we get any map to load

      if length(sTemp) > 0 then
        begin

          // Set this a new current directory

          MapDir := ExtractFilePath(sTemp);

          // Find if this is anything we can use

          sExt := ExtractFileExt(sTemp);
          if MapImageObj.InqMapTypeFromExt(sExt) <> mtNone then
            begin

              // Load this also
              
              sName := ExtractFileNamePart(sTemp);
              MapLoad (sName + sExt);
            end;
          MapListBoxLoad;

          MapSetDirDialog := true;
        end;
    end;
end;
//------------------------------------------------------------------------------
// If current item is of type Map, open it
//
procedure TTheMainForm.ActionOpenMapExecute(Sender: TObject);
var
  d, s : string;
  pDir : boolean;
begin
  if (MapItemCur <> nil) then
    if (CompareStr(MapItemCur.InqTypeName(), 'Map') = 0) then
      begin
        s := MapItemCur.InqName();

        // Strip of any ..(xxxx)

        pDir := false;
        if length(s) > 4 then
          if CompareStr(AnsiLeftStr(s,3), '..(') = 0 then
            begin
              s := AnsiMidStr(s,4,length(s) - 4);

              // Set the parent directory

              MapDirSet(ExtractParentDir(MapDir));
            end;

        // If this is a directory also, set it

        d := MapDir + s;
        if DirectoryExists(d) then
          MapDirSet(d);

        // Try to load it as a map in the current directory

        MapLoad(s + '.jpg');
      end;
end;
//------------------------------------------------------------------------------
// Load a picture and its data  (inMapName has no dir but ext)
//
procedure TTheMainForm.MapLoad (inMapName : string);
var
  mp : TMapItem;
  i  : integer;
  s  : string;
begin

  // Try Load the Map

  s := MapDir + inMapName;
  if not MapImageObj.Load(s) then
    begin
      Application.MessageBox (PAnsiChar('Map (' + s + ') does not exist!'),
                                'On loading Map');
      StatusBar.Panels[StatusApp].Text := 'Map (' + s + ') does not exist!';
      Exit;
    end;

  // Stop any higlighting of items

  DrawTimer.Enabled := false;
  
  // Desect any current item

  MapItemSelect (nil);

  // Remove any reference to copied item

  MapItemCopy := nil;

  // Clear all old items

  MapItemList.Clear;

  // Clear all old item types

  MapItemTypeList.Clear;

  StatusBar.Panels[StatusApp].Text := 'Map loaded';

  // Build all varables handling map names

  MapName := ExtractFileNamePart(inMapName);
  MapExt  := ExtractFileExt(inMapName);

  // Load the item types and items

  MapLoadFromFile(inMapName);

  // Set all item types to visible

  MapItemTypeList.SetVisibleAll(true);

  // Update all Item types menues

  ClearAllItemTypesInMenus;
  GuiUpdateMapItemTypes;

  // Walk all Map Points and add them to listbox

  MapItemListBoxLoad;

  // Set the map name in the status bar

  StatusBar.Panels[StatusMap].Text := MapName;

  // Update all Gui

  GuiUpdateMapScroll;
  GuiUpdateMapScale;
  GuiUpdateItemSelection;

  // Save all types into the Ini file types array

  for i := 0 to MapItemList.InqPoints() - 1 do
    begin

      // Get the pointer to the Map Point

      mp := MapItemList.InqPoint(i);
      if (mp <> nil) then
        if (mp.InqType() <> nil) then
          IniSaveLastItemsAdd (mp.InqType());
    end;

  // Set the current map to selected if it still is in the list

  SelectMapTimer.Enabled := true;
end;
//------------------------------------------------------------------------------
// Save a Map to file
//
procedure TTheMainForm.MapSaveToFile;
var
  F        : TextFile;
  sMapName : string;
begin

  // Test if filename is ok and there are any point

   if length(MapName) < 1 then
    begin
      Application.MessageBox('Map has no name', 'On save');
      Exit;
    end;

  // Change the ext of it

  sMapName := MapDir + MapName + '.xxx'; // MapItemExt;

  // Create the file

  AssignFile (F, sMapName);
  Rewrite(F);

  //--- Write Map data info

    WriteLn(F, '<' + LeafGetName(LeafsMap,LeafMapInfo) + '=');

    // Save the item type

    WriteLn(F, '<' + LeafGetName(LeafsMap,LeafMapName) + '=' +
                     MapName +
               '>');

    // Put the end

    WriteLn(F, '>');

  //--- Write all Item Types ------------------------

    // Put the header

    WriteLn(F, '<' + LeafGetName(LeafsMap,LeafItemTypeList) + '=');

    // Save the item type

    MapItemTypeList.SaveToFile(F);

    // Put the end

    WriteLn(F, '>');


  //--- Write all Item  ------------------------

    // Put the header

    WriteLn(F, '<' + LeafGetName(LeafsMap,LeafItemList) + '=');

    // Save the item type

    MapItemList.SaveToFile(F);

    // Put the end

    WriteLn(F, '>');


  CloseFile(F);
end;

//------------------------------------------------------------------------------
// Load a Map from File (sMapName = Numenor.jpg)
//
procedure TTheMainForm.MapLoadFromFile (sMapName : string);
var
  F           : TextFile;
  sBuf        : string;
  sFileName   : string;
  sMapNameExt : string;
  id          : integer;
begin

  sFileName   := MapDir + sMapName;
  sBuf        := ExtractFileNamePart(sMapName);
  sMapNameExt := ExtractFileExt(sMapName);
  sFileName   := MapDir + sBuf + '.xxx';

  // Open the File and read its input and build the array with MapPoints

  if (Length(sFileName) = 0) or (not FileExists(sFileName)) then
    Exit;

  AssignFile (F, sFileName);
  Reset(F);

  while not Eof(F) do
    begin
      // Get the first object

      Readln(F, sBuf); // Syntax : <objectname=

      id := LeafGetId(LeafsMap, LeafGetObjectName(sBuf));
      case id of
        LeafObjectAtEnd  : break;
        //LeafMapInfo    :
        LeafItemTypeList : MapItemTypeList.LoadFromFile(F);
        LeafItemList     : MapItemList.LoadFromFile (F, MapItemTypeList);
      else
        // Unknown object, skip it
        LeafSkipObject(F);
      end;
    end;

  // Close file

  CloseFile(F);
end;
//------------------------------------------------------------------------------
// Dispose and clear of any Maps loaded
//
procedure TTheMainForm.MapListBoxEmpty;
var
  ListItem : TListItem;
  i : integer;
begin

  // First dispose of anly old Map Info record

  for i := 0 to MapListBox.Items.Count - 1 do
    begin
      ListItem := MapListBox.Items[i];
      if ListItem <> nil then
        if ListItem.Data <> nil then
          Dispose(ListItem.Data)
    end;

  MapListBox.Items.Clear;
end;
//------------------------------------------------------------------------------
// List all Maps in MapListBox listbox
//
procedure TTheMainForm.MapListBoxLoad;
var
  ListItem   : TListItem;
  sr         : TSearchRec;
  FileAttrs  : Integer;
  bAnyParent : boolean;
begin
  bAnyParent := false;

  // First dispose of anly old Map Info record

  MapListBox.Selected := nil;
  MapListBoxEmpty;

  // Find the directory of the application

  if DirectoryExists(MapDir) then
    begin

      // Walk this directory for all JPEG files

      FileAttrs := faAnyFile + faReadOnly + faHidden + faSysFile + faVolumeID +
                    faDirectory + faArchive + faSymLink	;

      if MapDir[length(MapDir)] <> '\' then
        MapDir := MapDir + '\';

      self.Caption := 'The Map Tool [' + MapDir + ']';

      if FindFirst(MapDir + '*.*', FileAttrs, sr) = 0 then
        begin
          repeat
            begin
              // Is it a file, try to use it

              if ((sr.Attr and faDirectory) <> 0) and
                       (CompareStr(sr.Name,'.') <> 0) then
                begin
                  ListItem := MapListBox.Items.Add;
                  if CompareStr(sr.Name,'..') = 0 then
                    begin
                      ListItem.Caption := MapDirParent;
                      ListItem.ImageIndex := 24;
                      bAnyParent := true;
                    end
                  else
                    begin
                      ListItem.Caption := sr.Name;
                      ListItem.ImageIndex := 4;
                    end
                end
              else if (sr.Attr and FileAttrs) = sr.Attr then
                begin
                  // Use this file

                  MapListBoxLoadThis (sr.Name);
                end;
              
            end;
          until FindNext(sr) <> 0;
          FindClose(sr);

        end;
    end;

  // If not any parent directory, add desktop

  if not bAnyParent then
    begin
      ListItem := MapListBox.Items.Add;
      ListItem.Caption := MapDirDesktop;
      ListItem.ImageIndex := 24;
    end;

  MapListBoxSetWdt;

  // Set the current map to selected if it still is in the list

  SelectMapTimer.Enabled := true;
end;
//------------------------------------------------------------------------------
// List all Maps in MapListBox listbox
//
procedure TTheMainForm.MapListBoxLoadThis (fileName : string);
var
  ListItem : TListItem;
  fExt     : string;
  pInfoRec : pTMapInfo;
  mtType   : TMapType;
begin

  // Get the file extension

  fExt := ExtractFileExt(fileName);
  mtType := MapImageObj.InqMapTypeFromExt(fExt);
  if mtType <> mtNone then
    begin
      // Create a map info record

      New(pInfoRec);
      pInfoRec^.MapName := ExtractFileNamePart(fileName);
      pInfoRec^.MapType := mtType;
      pInfoRec^.MapExt  := fExt;

      // Place the map in listbox

      ListItem := MapListBox.Items.Add;
      ListItem.Caption := ExtractFileNamePart(fileName);
      ListItem.ImageIndex := 26;

      case mtType of
        mtJpeg : fExt := 'Jpeg';
        mtGif  : fExt := 'Gif';
        mtBmp  : fExt := 'Bmp';
      end;

      ListItem.SubItems.Add(fExt);

      // Place the info structure as Data (DONT forgett to dispose it)

      ListItem.Data := pInfoRec;
    end;
end;
//------------------------------------------------------------------------------
// Update the widths of the columns
//
procedure TTheMainForm.MapListBoxSetWdt ;
var
  i, w1, w2 : integer;
  li        : TListItem;
begin
  w1 := 40;
  w2 := 40;

  for i := 0 to MapListBox.Items.Count do
    begin
      li := MapListBox.Items[i];
      if li <> nil then
        begin
          w1 := Max(w1, MapListBox.StringWidth(li.Caption + 'xxx'));
          if li.SubItems.Count > 0 then
            w2 := Max(w2, MapListBox.StringWidth(li.SubItems[0] + 'xxx'));
        end;
    end;

  MapListBox.Column[0].Width := w1;
  MapListBox.Column[1].Width := w2;
end;
//------------------------------------------------------------------------------
// Mark map as seleted
//
procedure TTheMainForm.MapListBoxSelectCurMap;
var
  ListItem : TListItem;
begin
  Exit;
  // Set the current map to selected if it still is in the list

  if (length(MapName) > 0) and (MapListBox.Items.Count > 0)  then
    begin
      ListItem := MapListBox.FindCaption (0, MapName,false,true,false);
      if ListItem <> nil then
        begin
          // Make the item visible

          ListItem.MakeVisible (false);

          // Set description also

          ListItem.SubItems.Delete(0);
          ListItem.SubItems.Add(MapImageObj.InqDesc());
          MapListBoxSetWdt;

          // Select it

          MapListBox.ItemIndex := ListItem.Index;
          MapListBox.Invalidate;

        end;
    end;
end;
//------------------------------------------------------------------------------
// User change the Map
//
procedure TTheMainForm.MapListBoxClick(Sender: TObject);
var
  s : string;
  pInfoRec : pTMapInfo;
  i : integer;
  DirDialog : TForm1;
  Item : TListItem;
begin

  // Make sure its a new item that is selected

  Item := MapListBox.Selected;

  if (Item <> nil) then

    // Make sure its another map that is selected

    if StrComp(PAnsiChar(Item.Caption), PAnsiChar(MapName)) <> 0 then
      begin
        // Let the user save any old map

        MapSaveOld;

        // Handle the selection now

        if Item.Data <> nil then
          begin

            // There is a map somewhere here, Load it

            pInfoRec := Item.Data;
            s := pInfoRec^.MapName + pInfoRec^.MapExt;
            MapLoad (s);
          end
        else if CompareStr(Item.Caption, MapDirParent) = 0 then
          begin

            // This is a parent directory, set it

            for i := Length(MapDir) - 1 downto 1 do
              begin
                if MapDir[i] = '\' then
                  begin
                    MapDir := AnsiLeftStr(MapDir, i);
                    break;
                  end;
              end;

            MapListBoxLoad;
            Exit;
          end
        else  if CompareStr(Item.Caption, MapDirDesktop) = 0 then
          begin

            // This is desktop, open dialog and go on

            DirDialog := TForm1.Create(self);

            DirDialog.ShowModal;

            if DirDialog.bOk then
              s := DirDialog.ResultDir.Text
            else
              s := '';

            DirDialog.Free;

            if length(s) > 0 then
              begin
                MapDir := s + '\';
                MapListBoxLoad;
                Exit;
              end;
          end
        else
          begin

            // So it must be a directory, set that

            if MapDir[Length(MapDir)] = '\' then
              MapDir := MapDir + Item.Caption + '\'
            else
              MapDir := MapDir + '\' + Item.Caption + '\';

            MapListBoxLoad;
          end;
      end;
end;
//------------------------------------------------------------------------------
// Save old Map if dirty and user wants to
//
procedure TTheMainForm.MapSaveOld;
var
  r : integer;
  s : string;
begin

  // Test if the map needs to be saved

  if MapItemList.InqDirty or MapItemNameEditing then
    begin
      s :=  'You have changed ' + MapName +
                  '. Do you want to save it?';
      r := Application.MessageBox (PAnsiChar(s),
              'Closing Map',MB_YESNO);
      if (r = IDYES) then
        begin
          ActionSaveMapExecute(nil);
        end;
    end;
end;
//------------------------------------------------------------------------------
// Set new Map directory  (this is an absolute dir and must exist)
//
function TTheMainForm.MapDirSet(inDir : string): boolean;
begin
  MapDirSet := false;

  // Make sure its a new item that is selected

  if DirectoryExists(inDir) then
    begin

    // Make sure user has a chanse to save old map

    MapSaveOld;

    // Set it as current map

    MapDir := inDir;

    // Load al maps in the directory

    MapListBoxLoad;
    MapDirSet := true;
  end;
end;
//------------------------------------------------------------------------------
// Resets the selection of the Map list box
//
procedure TTheMainForm.SelectMapTimerTimer(Sender: TObject);
begin
  MapListBoxSelectCurMap;
  SelectMapTimer.Enabled := false;
end;
//------------------------------------------------------------------------------
// Set Hint text of the map
//
procedure TTheMainForm.MapListBoxInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: String);
begin
  InfoTip := 'Map:' + MapDir + Item.Caption;
end;
//------------------------------------------------------------------------------
//                        Map Item general functions
//------------------------------------------------------------------------------
// Set this item as selected and handle old selection
//
procedure TTheMainForm.MapItemSelect (mp : TMapItem);
var
  sTemp : string;
  i     : integer;
  r     : TRect;
begin

  // First handle any old current item (no matter new item)

  if (MapItemCur <> mp) and (MapItemCur <> nil) then
    begin

      // Update description

      if MapItemDesc.Modified then
        begin
          // Convert description to a string

          sTemp := '';
          for i := 0 to MapItemDesc.Lines.Count do
            sTemp := sTemp + MapItemDesc.Lines[i];

          // Update the Map Item

          MapItemList.ItemDescUpdate (MapItemCur, sTemp, udEditDesc);
        end;
    end;

  // Redraw map to erase old selections

  if MapItemCur <> nil then
    MapImageObj.BaseDraw;

  // Ok, now we do the new stuff

  MapItemCur := mp;

  if MapItemCur <> nil then
    begin

      // Update the description text box

      SetDesc(MapItemCur.InqDesc());

      // Move the Map into this position also

      r.Left := 0;
      r.Top := 0;
      r.Right := Map.Width;
      r.Bottom := Map.Height;

      if not IsPntInRect(MapImageObj.CnvMapPntToScr(MapItemCur.InqMidPoint()),r) then
        MapImageObj.MoveMapPntCenScr (MapItemCur.InqMidPoint());

      // Draw a red circle around the position

      MapItemCur.Draw(MapImageObj, false);

      // Set the staus bar text

      StatusBar.Panels[StatusItem].Text := MapItemCur.InqName();
    end;

  GuiUpdateItemSelection;
end;
//------------------------------------------------------------------------------
//                            Map Item List Box
//------------------------------------------------------------------------------
// List all Map Points in MapItemListBox listbox
//
procedure TTheMainForm.MapItemListBoxLoad;
var
  i     : integer;
  mp    : TMapItem;
begin

  // Clear old things from the listbox

  MapItemListBox.Clear;

  // Walk all Map Points and add them to listbox

  for i := 0 to MapItemList.InqPoints() - 1 do
    begin

      // Get the pointer to the Map Point

      mp := MapItemList.InqPoint(i);

      // Test if this point should be added

      if mp <> nil then
        MapItemListBoxAdd(mp);
    end;

  // Set some usefull widths of the columns

  MapItemListBoxSetWdt;

  // Set the last and current point selected to nil

  MapItemListBoxSel (MapItemCur);

  GuiUpdateMapItemTypes;
end;
//------------------------------------------------------------------------------
// Find the item that is selected
//
function TTheMainForm.MapItemListBoxInqItem : TMapItem;
begin
  MapItemListBoxInqItem := nil;

  if MapItemListBox.Items.Count > 0 then
    if MapItemListBox.SelCount = 1 then
      MapItemListBoxInqItem :=
        MapItemListBox.Items[MapItemListBox.Selected.Index].Data;
end;
//------------------------------------------------------------------------------
// Set a new name of an item
//
procedure TTheMainForm.MapItemListBoxSetName (mp : TMapItem);
var
  i  : integer;
begin
  if mp <> nil then
    for i := 0 to MapItemListBox.Items.Count - 1do
      if MapItemListBox.Items[i].Data = mp then
        begin
          MapItemListBox.Items[i].Caption := mp.InqName();
          Exit;
        end;
end;
//------------------------------------------------------------------------------
// Add one item to the listbox
//
procedure TTheMainForm.MapItemListBoxAdd (mp : TMapItem);
var
  li : TListItem;
begin
  if mp <> nil then
    if MapItemList.InqPointIsVisible(mp) then
      begin

        // Add itemname to the listbox

        li := MapItemListBox.Items.Add;

        // Set name and type

        li.Caption := mp.InqName;
        li.SubItems.Add(mp.InqTypeName);
        li.Data := mp;
      end;
end;
//------------------------------------------------------------------------------
// Remove one item to the listbox
//
procedure TTheMainForm.MapItemListBoxDel (mp : TMapItem);
var
  i : integer;
begin
  if mp <> nil then
    begin
      for i := 0 to MapItemListBox.Items.Count - 1do
        if MapItemListBox.Items[i].Data = mp then
          begin
            MapItemListBox.Items.Delete (i);
            break;
          end;
    end;
end;
//------------------------------------------------------------------------------
// Set this item selected in listbox
//
procedure TTheMainForm.MapItemListBoxSel (mp : TMapItem);
var
  i : integer;
begin
  if mp <> nil then
    begin
      for i := 0 to MapItemListBox.Items.Count - 1do
        if MapItemListBox.Items[i].Data = mp then
          begin
            MapItemListBox.Items[i].MakeVisible(false);
            MapItemListBox.ItemIndex := i;
            Exit;
          end;
    end;
end;
//------------------------------------------------------------------------------
// Update the widths of the columns
//
procedure TTheMainForm.MapItemListBoxSetWdt ;
var
  i, w1, w2 : integer;
  li        : TListItem;
begin
  w1 := 40;
  w2 := 40;

  for i := 0 to MapItemListBox.Items.Count do
    begin
      li := MapItemListBox.Items[i];
      if li <> nil then
        begin
          w1 := Max(w1, MapItemListBox.StringWidth(li.Caption + 'xxx'));
          if li.SubItems.Count > 0 then
            w2 := Max(w2, MapItemListBox.StringWidth(li.SubItems[0] + 'xxx'));
        end;
    end;

  MapItemListBox.Column[0].Width := w1;
  MapItemListBox.Column[1].Width := w2;
end;
//------------------------------------------------------------------------------
// User clicked an item in the Map Point List
//
procedure TTheMainForm.MapItemListBoxSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and (Item <> nil) then
    if Item.Data <> nil then
      begin
        DrawNifty (Item.Data);
        MapItemSelect (Item.Data);
      end;
end;
//------------------------------------------------------------------------------
// User stopped editing the Map item point name
//
procedure TTheMainForm.MapItemListBoxEdited(Sender: TObject;
  Item: TListItem; var S: String);
var
  mi : TMapItem;
begin
  MapItemNameEditing := false;

  // Find the item that is changed (it might not be the current item)

  if (Item.Data <> nil) then
    begin
      mi := TMapItem(Item.Data);
      MapItemList.ItemNameUpdate (mi, S, udEditName);

      GuiUpdateItemSelection;
    end
  end;
//------------------------------------------------------------------------------
// User is now editint the Map Item Name
//
procedure TTheMainForm.MapItemListBoxEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  MapItemNameEditing := true;
  AllowEdit := true;
end;
//------------------------------------------------------------------------------
// Set Hint for any item in the list
//
procedure TTheMainForm.MapItemListBoxInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: String);
var
  mp   : TMapItem;
begin
  InfoTip := 'Click on item to see where it is';

  // Get the MapDataPont of the selection in the Map Point Listbox

  if Item <> nil then
    if (Item.Data <> nil) then
      begin
        mp := Item.Data;
        InfoTip := mp.InqName + ' (' + mp.InqTypeName() + ', ' +
                      mp.InqGeometryName() + ', Points ' +
                      IntToStr(mp.InqPointLen) + ')';
      end;
end;
//------------------------------------------------------------------------------
// Set the current item selected and visible again
//
procedure TTheMainForm.MapItemTimerTimer(Sender: TObject);
begin
  MapItemListBoxSel (MapItemCur);
  MapItemTimer.Enabled := false;
end;
//------------------------------------------------------------------------------
// Set new description
//
procedure TTheMainForm.SetDesc(desc : string);
begin
  MapItemDesc.Lines.Clear;
  MapItemDesc.Lines.Add (desc);
  MapItemDesc.SelStart := 0;
  MapItemDesc.SelLength := 0;
  MapItemDesc.Modified := false;
end;
//------------------------------------------------------------------------------
//                             Mouse and Cursor
//------------------------------------------------------------------------------
// User pressed one of the mouse buttons in the Map window
//
procedure TTheMainForm.MapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  // We remember this position to later understand where the user
  // started it all

  MouseDownLastPos.X := X;
  MouseDownLastPos.Y := Y;
end;
//------------------------------------------------------------------------------
// User moved the mouse in the Map window
//
procedure TTheMainForm.MapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if EditMode then
    MapMouseEditMove (Sender, Shift,X, Y)
  else
    MapMousePointMove(Sender, Shift,X, Y);

  CursorTimer.Enabled := true;
end;
//------------------------------------------------------------------------------
// User released one of the mouse buttons in the Map window
//
procedure TTheMainForm.MapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  if EditMode then
      begin
        if not MapMouseItemChanged (Shift) then
          if not MapItemMoveStarted then
            if (not MapMouseInqScrollStopped()) then
              if (not MapMouseSelectItem(Shift, X,Y)) then
                MapMouseScaleMap(Button, Shift, X, Y);

      end
    else
      begin
        if (not MapMouseInqScrollStopped()) then
          if (not MapMouseSelectItem(Shift, X,Y)) then
            MapMouseScaleMap(Button, Shift, X, Y);

      end;

  MapItemMoveType := msNone;
  MapItemMoveStarted := false;

end;
//------------------------------------------------------------------------------
// User moved the mouse in the Map window
//
function TTheMainForm.MapMouseScrollMove(Shift: TShiftState; X,
                                          Y: Integer) : boolean;
begin
  MapMouseScrollMove := false; // No scrolling so far

  // Calc if this is a beginning of a map drag

  if not MapMoveStarted and (ssLeft in Shift) then
    begin
      // Mark that the mouse move has started

      MapMoveStarted := true;

      // Calculate the start point of the move

      MapMoveStartPos.X := X;
      MapMoveStartPos.Y := Y;

      // Set the Hand cursor as new cursor

      //if (Map.Cursor <> crMapDrag) then
          Screen.Cursor := crMapDrag;

      MapMouseScrollMove := true;
    end

  // Calc if this is a moving map, i.e. the left button is down

  else if (ssLeft in Shift) then
    begin

      // Move the map

      MapImageObj.MoveScrDist (X - MapMoveStartPos.X, Y - MapMoveStartPos.Y);

      // Remember this as last position

      MapMoveStartPos.X := X;
      MapMoveStartPos.Y := Y;

      MapMouseScrollMove := true;
    end;
end;
//------------------------------------------------------------------------------
// Test if scroll just stopped (used on mouse up)
//
function TTheMainForm.MapMouseInqScrollStopped : boolean;
begin

  // Find out if the user just stopped scrolling

  if MapMoveStarted then
    begin
      MapMoveStarted := false;

      // Change back to the default cursor

      if Screen.Cursor = crMapDrag then
        Screen.Cursor := crMapDefault;

      // Update the scroll controls to reflect the new position of map

      GuiUpdateMapScroll;
      GuiUpdateItemSelection;

      MapMouseInqScrollStopped := true;
    end
  else
    MapMouseInqScrollStopped := false;

end;
//------------------------------------------------------------------------------
// Test if item is under cursor
//
function TTheMainForm.MapMouseTestItem (Shift: TShiftState; X, Y: Integer) : boolean ;
var
  pMapPoint : TMapItem;
begin
  pMapPoint := nil;

  if ( not (ssCtrl in Shift)) then
    begin

      // See if any map points should be showed

      pMapPoint := MapItemList.InqPoint(MapImageObj.CnvScrPntToMap(X,Y));

      if pMapPoint = nil then
        begin

          // Reset Item selected cursor if on

          if Screen.Cursor = crMapItem then
            Screen.Cursor := crMapDefault;
        end
      else
        begin

          // Set the cursor showing some item is under it

          if (Screen.Cursor <> crMapItem) then
              Screen.Cursor := crMapItem;

        end;
    end
  else if Screen.Cursor = crMapItem then
    Screen.Cursor := crMapItem;

  if pMapPoint <> nil then
    StatusBar.Panels[StatusApp].Text := 'Item ' + pMapPoint.InqName()
  else
    StatusBar.Panels[StatusApp].Text := '';

  // Return true if any item was found

  MapMouseTestItem := (pMapPoint <> nil);
end;
//------------------------------------------------------------------------------
// Select item under cursor
//
function TTheMainForm.MapMouseSelectItem (Shift: TShiftState; X, Y: Integer) : boolean;
var
  mp   : TMapItem;
begin
  MapMouseSelectItem := false;

  if ( not (ssCtrl in Shift)) then
    begin

      // Find out if user is on any Map Item, then select it

      mp := MapItemList.InqPoint(MapImageObj.CnvScrPntToMap(MouseDownLastPos));
      if mp <> nil then
        begin
          MapItemListBoxSel (mp);
          MapItemSelect (mp);
          MapMouseSelectItem := true;
        end;
    end
end;
//------------------------------------------------------------------------------
// User released one of the mouse buttons in the Map window
//
procedure TTheMainForm.MapMouseScaleMap(Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
var
  pMap : TPoint;
begin

  // Scale up if left button, down if right button

  if (mbLeft = Button) then
    begin
      pMap := MapImageObj.CnvScrPntToMap(MouseDownLastPos);
      MapImageObj.Scale([drUp], MouseDownLastPos);
      MapImageObj.MoveMapPntToScrPnt(pMap,MouseDownLastPos);
      GuiUpdateMapScale;
      GuiUpdateItemSelection
    end
  else if (mbRight = Button) then
    begin
      pMap := MapImageObj.CnvScrPntToMap(MouseDownLastPos);
      MapImageObj.Scale([drDown], MouseDownLastPos);
      MapImageObj.MoveMapPntToScrPnt(pMap,MouseDownLastPos);
      GuiUpdateMapScale;
      GuiUpdateItemSelection
    end;
end;
//------------------------------------------------------------------------------
// User moved the mouse in the Map window
//
procedure TTheMainForm.MapMousePointMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

  // Handle any scrolling first, Test if any item is under cursor

  if not MapMouseScrollMove (Shift,X,Y) then
    if not MapMouseTestItem (Shift, X,Y) then
      if (Screen.Cursor <> crMapDefault) then
        Screen.Cursor := crMapDefault;

  // Show position in the map

  StatusBar.Panels[StatusPos].Text := PntToStr(MapImageObj.CnvScrPntToMap(X,Y));
end;
//------------------------------------------------------------------------------
// User moved the mouse in the Map window
//
procedure TTheMainForm.MapMouseEditMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

  // Test if item is moving; if scrolling map, hit any item edit frame
  // and last if another item

  if not MapMouseItemMoving (Shift,X,Y) then
    if not MapMouseScrollMove (Shift,X,Y) then
      if not MapMouseTestHitItem (Shift,X,Y) then
        if not MapMouseTestItem (Shift, X,Y) then
          if (Screen.Cursor <> crMapDefault) then
            Screen.Cursor := crMapDefault;

  StatusBar.Panels[StatusPos].Text := PntToStr(MapImageObj.CnvScrPntToMap(X,Y));
end;
//------------------------------------------------------------------------------
// Test if the cursor is over any selected item
//
function TTheMainForm.MapMouseTestHitItem(Shift: TShiftState; X,Y: Integer):boolean;
begin

  if (MapItemCur <> nil) then
    begin

      // Find out if any item edit point is selected

      MapItemMoveType := MapItemCur.InqAtPosSelection(
                      MapImageObj.CnvScrPntToMap(X,Y),
                      MapImageObj.InqScale, MapItemMovePointIndex);

      // Set the right cursor for the occation

      // loaded cursors constants
      // crMapDefault    = 1; // Default cursor for Map
      // crMapDrag       = 2; // Used when dragging / scrolling map
      // crMapRotate     = 3; // Used when rotating an item
      // crMapPointMove  = 4; // Used when moving a point
      // crMapMidMove    = 5; // Used when moving an item
      // crMapPointAdd   = 6; // Used when adding a point
      // crMapScale      = 7; // Used when scaling a point
      // crMapScaleLu    = 8; // Used when scaling Left Up
      // crMapScaleLd    = 9; // Used when scaling Left Down
      // crMapItem       = 10; // Used when found an item
      // crMapPointDel   = 11; // Used when deleting a map point

      case MapItemMoveType of
        msMove:  Screen.Cursor := crMapMidMove;
        msSclLU: Screen.Cursor := crMapScaleLu;
        msSclLD: Screen.Cursor := crMapScaleLd;
        msSclRU: Screen.Cursor := crMapScaleLd;
        msSclRD: Screen.Cursor := crMapScaleLu;
        msRot:   Screen.Cursor := crMapRotate;
        msPoint:
          begin
            if (ssCtrl in Shift) and (MapItemCur.InqPointLen() >= 3) then
              Screen.Cursor := crMapPointDel
            else
              Screen.Cursor := crMapPointMove;
          end;
        msLine:
          begin
            Screen.Cursor := crMapPointAdd;
            StatusBar.Panels[StatusApp].Text := 'Item ' + MapItemCur.InqName() +
                    ' (Index ' + IntToStr(MapItemMovePointIndex) + ')';
          end;
        msNone:
          begin

            // Remove any old cursor

            if (Screen.Cursor = crMapMidMove) or
               (Screen.Cursor = crMapScaleLu) or
               (Screen.Cursor = crMapScaleLd) or
               (Screen.Cursor = crMapRotate) or
               (Screen.Cursor = crMapPointMove) or
               (Screen.Cursor = crMapPointAdd) or
               (Screen.Cursor = crMapPointDel) then
              Screen.Cursor := crMapDefault;
          end;
      end;
    end;

  // return true if any selected frame part was hit

  MapMouseTestHitItem := (MapItemMoveType <> msNone);
end;
//------------------------------------------------------------------------------
// The user moves, scales or rotates an item (perhaps)
//
function TTheMainForm.MapMouseItemMoving(Shift: TShiftState; X,Y: Integer) : boolean;
var
  p2 : TPoint;
  nAng   : real;
  d1, i : integer;
begin
  MapMouseItemMoving := false; // No points are moving so far

  // Calc if this is a beginning of a item move

  if (not MapItemMoveStarted) and      // Not started yet
     (MapItemCur <> nil) and          // Item is really selected
     (ssLeft in Shift) and             // left mouse button
     (MapItemMoveType <> msNone) then  // Over some part of the edit frame
    begin
      // Mark that the mouse move has started

      MapItemMoveStarted := true;

      // We are actually moving now (tell the next move about it)

      MapMouseItemMoving := true;

      // Calc move coordintes

      MapItemMoveStartPos   := MapImageObj.CnvScrPntToMap(X,Y);
      MapItemMoveLastDist.X := 0;
      MapItemMoveLastDist.Y := 0;

      // Calc rotation point and start angle

      MapItemMoveItemExt  := MapItemCur.InqExt();
      MapItemMoveMidPoint := InqMidOfRect(MapItemMoveItemExt);
      MapItemMoveStartAng := InqAng (MapItemMoveMidPoint,MapItemMoveStartPos);
      MapItemMoveLastAng  := 0.0;

      // Calculate scale

      MapItemScaleStartPos.X  := abs(MapItemMoveStartPos.X - MapItemMoveMidPoint.X);
      MapItemScaleStartPos.Y  := abs(MapItemMoveStartPos.Y - MapItemMoveMidPoint.Y);
      MapItemScaleLastSclX  := 1.0;
      MapItemScaleLastSclY  := 1.0;

      // create new point

      MapItemMovePointFirst := false;
      if (MapItemMoveType = msLine) then
        begin
          // Create a new point at this position between atindex and +1

          //MapItemCur.AddPoint(MapItemMovePointIndex+1, MapImageObj.CnvScrPntToMap(X,Y));
          MapItemMovePointIndex := MapItemMovePointIndex + 1;
          MapItemList.ItemPntNew(MapItemCur, MapItemMovePointIndex,
                              MapImageObj.CnvScrPntToMap(X,Y), udNewPnt);
        end
      else
        begin

          // Remember first position of this moved point

          MapItemMovePointFirst := true; // First

        end;

      // Redraw map, item and edit frame

      MapImageObj.BaseDraw;

      // If highligt redraw all but the selected

      if HighlightAllItems then
        for i := 0 to MapItemList.InqPoints() -1 do
          if MapItemList.InqPoint(i) <> MapItemCur then
            MapItemList.InqPoint(i).Draw(MapImageObj, false, false,1);

      // Draw the selected

      MapItemCur.Draw(MapImageObj, true, true);

      // Draw edit frame
      
      MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);
    end

  // Calc if this is a moving item, i.e. the left button is down

  else if MapItemMoveStarted and   // Item move has really started
         (MapItemCur <> nil) and  // Still an item selected
         (ssLeft in Shift) then    // Still left button
    begin

      // Depending on item type selected do some nice moves...

      case MapItemMoveType of
        msMove:
          begin
            //------------------ Move the item --------------------

            // Set the old the Matrix and draw

            MapImageObj.SetMatrix (MapItemMoveMidPoint,
                  1.0,1.0,0.0,MapItemMoveLastDist);

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);

            // Calc the difference from last drag position

            p2 :=  MapImageObj.CnvScrPntToMap(X,Y);
            MapItemMoveLastDist.X := p2.X - MapItemMoveStartPos.X;
            MapItemMoveLastDist.Y := p2.Y - MapItemMoveStartPos.Y;

            // Set the new the Matrix and draw

            MapImageObj.SetMatrix (MapItemMoveMidPoint,
                  1.0,1.0,0.0,MapItemMoveLastDist);

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);

            // Set back matrix

            MapImageObj.SetMatrix();

            StatusBar.Panels[StatusApp].Text := 'Move item X ' +
                IntToStr(MapItemMoveLastDist.X) + ' Y ' +
                IntToStr(MapItemMoveLastDist.Y) ;
          end;
        msSclLU, msSclLD, msSclRU, msSclRD:
          begin
            //------------ Scale the item around its mid point ----------

            // Set the old the Matrix and draw

            p2.X := 0;
            p2.Y := 0;
            MapImageObj.SetMatrix (MapItemMoveMidPoint,
                  MapItemScaleLastSclX,MapItemScaleLastSclY,0.0,p2);

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);

            // Calc the new scale from middle position

            p2 := MapImageObj.CnvScrPntToMap(X,Y);                  // Current

            // Use the old mouse pos as scale 1.0

            d1 := p2.X - MapItemMoveMidPoint.X; // current dist X
            if MapItemScaleStartPos.X = 0 then
              MapItemScaleLastSclX := 1.0
            else
              MapItemScaleLastSclX := d1 / MapItemScaleStartPos.X;

            if MapItemCur.InqPointLen() = 1 then
              MapItemScaleLastSclY := MapItemScaleLastSclX
            else
              begin
                d1 := p2.Y - MapItemMoveMidPoint.Y;
                if MapItemScaleStartPos.Y = 0 then
                  MapItemScaleLastSclY := 1.0
                else
                  MapItemScaleLastSclY := d1 / MapItemScaleStartPos.Y;
              end;

            p2.X := 0;
            p2.Y := 0;
            MapImageObj.SetMatrix (MapItemMoveMidPoint,
                  MapItemScaleLastSclX,MapItemScaleLastSclY,0.0,p2);

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);

            // Set back matrix

            MapImageObj.SetMatrix();

            StatusBar.Panels[StatusApp].Text := 'Scale X ' +
                FloatToStr(round(100 * MapItemScaleLastSclX)/100) + ' Y ' +
                FloatToStr(round(100 * MapItemScaleLastSclY)/100) ;

          end;
        msRot:
          begin
            //------------ Rotate the item round its midpoint --------------

            // Set the old the Matrix and draw

            p2.X := 0;
            p2.Y := 0;
            MapImageObj.SetMatrix (MapItemMoveMidPoint,1.0,1.0,MapItemMoveLastAng,p2);

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);

            // Calculate new angle

            nAng := InqAng (MapItemMoveMidPoint,
                             MapImageObj.CnvScrPntToMap(X,Y));

            // Set the new the Matrix and draw

            MapItemMoveLastAng := MapItemMoveStartAng - nAng;

            MapImageObj.SetMatrix (MapItemMoveMidPoint,1.0,1.0,MapItemMoveLastAng,p2);

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemMoveItemExt, MapItemCur.InqPointLen() > 1);

            // Set back matrix

            MapImageObj.SetMatrix();

            StatusBar.Panels[StatusApp].Text := 'Angle ' +
                FloatToStr(round(100 * 180 * MapItemMoveLastAng / Pi) / 100);

          end;
        msPoint, msLine:
          begin
            //-----------------     Move nearest point   ---------------------

            // Remember this first move for undo

            if MapItemMovePointFirst then
              begin
                MapItemList.ItemPntMove(MapItemCur,MapItemMovePointIndex, udMovePnt);
              end;

            MapItemMovePointFirst := false;

            // Draw item

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemCur.InqExt(),
                                    MapItemCur.InqPointLen() > 1);

            // Move the point

            MapItemCur.Move(MapItemMovePointIndex,
                            MapImageObj.CnvScrPntToMap(X,Y));

            MapItemCur.Draw(MapImageObj, true, true);
            MapImageObj.DrawEditFrame(MapItemCur.InqExt(),
                                    MapItemCur.InqPointLen() > 1);

            p2 := PntSubMove(MapImageObj.CnvScrPntToMap(X,Y),MapItemMoveStartPos);
            StatusBar.Panels[StatusApp].Text := 'Move point X ' +
                                      IntToStr(p2.X) + ' Y ' + IntToStr(p2.Y);

          end;
      end;

      // Double set all things...

      MapItemMoveStarted := true;
      MapMouseItemMoving := true;
    end;
end;
//------------------------------------------------------------------------------
// User changed an item (rotated, scaled, moved or deleted a point)
//
function TTheMainForm.MapMouseItemChanged (Shift: TShiftState): boolean;
var
  done : boolean;
begin
  done := false;

  // Chane a Map item cordinates

  case MapItemMoveType of
    msMove:
      begin

        // Move the item

        MapItemList.ItemMove(MapItemCur, MapItemMoveLastDist, udMoveItem);
        done := true;
      end;
    msRot:
      begin
        // Rotate the item

        MapItemList.ItemRotate(MapItemCur, MapItemMoveLastAng, udRotateItem);
        done := true;
      end;
    msSclLU, msSclLD, msSclRU, msSclRD:
      begin
        // Scale the item

        MapItemList.ItemScale(MapItemCur, MapItemScaleLastSclX,
                                      MapItemScaleLastSclY, udScaleItem);
        done := true;

      end;
    msPoint, msLine:
      begin
        if (ssCtrl in Shift) and (MapItemCur.InqPointLen() > 3) then
          begin
            // Delete the Map pont at index

            MapItemList.ItemPntDel(MapItemCur,MapItemMovePointIndex,udDelPnt);
            MapItemMovePointIndex := -1;
          end;

        done := true;
      end;
  end;

  if done then
    begin
      MapImageObj.BaseDraw;
      GuiUpdateItemSelection;
    end;

  MapMouseItemChanged := done;
end;
//------------------------------------------------------------------------------
// Make sure cursor is reloaded if outside map
//
procedure TTheMainForm.CursorTimerTimer(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      if (not MapImageObj.InqCursorInsideMap()) then
        begin
          Screen.Cursor := crDefault;
          CursorTimer.Enabled := false;
        end
    end
  else
    begin
      CursorTimer.Enabled := false;
    end
end;
//------------------------------------------------------------------------------
// Return the middle of the map position
//
function TTheMainForm.InqMapMid:TPoint;
begin
  InqMapMid.X := round(Map.Width / 2);
  InqMapMid.Y := round(Map.Height / 2);
end;
//------------------------------------------------------------------------------
//                                  Nifty
//------------------------------------------------------------------------------
// Action: Execute nifty on current item
//
procedure TTheMainForm.ActionShowNiftyExecute(Sender: TObject);
begin
  DrawNiftyOn := not DrawNiftyOn;
  GuiUpdateItemSelection;
end;
procedure TTheMainForm.ActionShowAllItemsExecute(Sender: TObject);
begin
  HighlightAllItems := not HighlightAllItems;
  GuiUpdateItemSelection;
end;

//------------------------------------------------------------------------------
// Draw nifty on any item
//
procedure TTheMainForm.DrawNifty(mi : TMapItem);
var
  LengthMm : integer;
begin
  // Draw the item a little nicer

  if DrawNiftyOn and (mi <> nil) then
    begin
      DrawNiftyIndex := 0;
      DrawNiftyItem := mi;
      DrawNiftyFirst := 0;
      DrawNiftyRand  := Random(100);
      DrawNiftyFrom  := true;

      case mi.InqGeometry of
       gtPoint:
        begin
          DrawNiftyLast := DrawNiftyRec.PointWdt;

          // Depending on map scale

          DrawNiftyRec.Increment := 2; //round(1/MapImageObj.InqScale());
        end;
       gtLine, gtCubic:
        begin
          // InqLength will give length of line in pixels
          // DrawNiftySpeed give the number of mm per sec to draw
          // DrawTimer.Interval will give interval between draw call in ms
          // Screen.PixelsPerInch will give ...
          // So... (instant headake)

          LengthMm := round(25.4 * mi.InqLength / Screen.PixelsPerInch);
          DrawNiftyLast := round(400 * (LengthMm / DrawNiftyRec.LineSpeed) /
                            DrawTimer.Interval);

          StatusBar.Panels[0].Text := mi.InqName() + ' Length: ' + IntToStr(LengthMm) + ' (mm)';

          //DrawNiftyLast := mi.InqLength div 6;
          DrawNiftyRec.Increment := 1;
        end;
       gtArea:
        begin
          if DrawNiftyRec.AreaPolyOn then
            DrawNiftyLast := 80
          else
            begin
              LengthMm := round(25.4 * mi.InqLength / Screen.PixelsPerInch);
              DrawNiftyLast := round(400 * (LengthMm / DrawNiftyRec.LineSpeed) /
                            DrawTimer.Interval);

              StatusBar.Panels[0].Text := mi.InqName() + ' Border: ' + IntToStr(LengthMm) + ' (mm)';
            end;

          DrawNiftyRec.Increment := 1;
        end;
      end;

      DrawTimer.Enabled := true;
    end
  else
    DrawNiftyItem := nil;
end;
//------------------------------------------------------------------------------
// Execute timer event on nifty drawing
//
procedure TTheMainForm.DrawTimerTimer(Sender: TObject);
var
  pos : TPoint;
  mid : TPoint;
begin
  if DrawNiftyIndex <= DrawNiftyLast then
    begin
      if DrawNiftyItem <> nil then
        begin

          pos := DrawNiftyItem.DrawNifty(MapImageObj,DrawNiftyFrom, DrawNiftyRand,
              DrawNiftyFirst, DrawNiftyIndex, DrawNiftyLast, DrawNiftyRec);

          DrawNiftyIndex := DrawNiftyIndex + DrawNiftyRec.Increment;

          // Test if point is inside the window

          if not DrawNiftyRec.LineMapCenOn then
            if not MapImageObj.InqMapPosInsideMap(pos) then
              begin
                // Move the map to center if outside

                MapImageObj.MoveMapPntCenScr(pos);
                DrawNiftyFrom := false;
              end
            else
              DrawNiftyFrom := true
          else
            begin
              DrawNiftyFrom := true;

              // Move tham map to center always

              mid := MapImageObj.CnvScrPntToMap(InqMapMid());

              if pos.X < mid.X then
                begin
                  DrawNiftyFrom := false;
                end
              else if pos.X > mid.X then
                begin
                  DrawNiftyFrom := false;
                end;

              if pos.Y < mid.Y then
                begin
                  DrawNiftyFrom := false;
                end
              else if pos.Y > mid.Y then
                begin
                  DrawNiftyFrom := false;
                end;

              if not DrawNiftyFrom then
                MapImageObj.MoveMapPntCenScr(pos);
            end;
        end
      else
        DrawTimer.Enabled := false;
    end
  else
    DrawTimer.Enabled := false;

  if (not DrawTimer.Enabled) and (MapItemCur <> nil) then
    begin
      // Draw the item

      MapItemCur.Draw(MapImageObj, EditMode);

      // Draw an edit frame

      if EditMode then
        MapImageObj.DrawEditFrame(MapItemCur.InqExt, MapItemCur.InqPointLen() > 1);

    end;
end;
//------------------------------------------------------------------------------
//                              Action List Code
//------------------------------------------------------------------------------
// User closed the application
//
procedure TTheMainForm.ActionExitExecute(Sender: TObject);
begin
  TheMainForm.Close
end;
//------------------------------------------------------------------------------
// Action: Set new color for item
//
procedure TTheMainForm.ActionSetItemColorExecute(Sender: TObject);
begin
  // Set the color for highlighting items

  if MapItemCur <> nil then
    begin
      ItemColorDlg.Color := MapItemCur.InqType.InqColor();
      if ItemColorDlg.Execute() then
        begin
          MapItemCur.InqType.SetColor (ItemColorDlg.Color);
          GuiUpdateItemSelection;
        end;
    end;
end;
//------------------------------------------------------------------------------
//                                Font Settings
//------------------------------------------------------------------------------
// Action: Set Font
//
procedure TTheMainForm.ActionSetFontExecute(Sender: TObject);
begin
  FontDialog.Font := MapListBox.Font;
  if FontDialog.Execute() = true then
    begin
      Self.Font := FontDialog.Font;
      FontSetCombo;
      GuiUpdateControls (0);
    end;
end;
//------------------------------------------------------------------------------
// Action: Set Font Name
//
procedure TTheMainForm.ActionSetFontNameExecute(Sender: TObject);
begin
 Self.Font.Name := FontCombo.Text;

 FontComboHeight.Font.Name := Self.Font.Name;
 GuiUpdateControls (0);
end;
//------------------------------------------------------------------------------
// Action: Set Font Height
//
procedure TTheMainForm.ActionSetFontHeightExecute(Sender: TObject);
begin
  Self.Font.Size := (StrToInt(FontComboHeight.Text));
  GuiUpdateControls (0);
end;
//------------------------------------------------------------------------------
//  Set the font info in the font combo boxes
//
procedure TTheMainForm.FontInitCombo;
begin

  // Load the Fond Combo Box with all things and set current values

  FontCombo.Items := Screen.Fonts;
  FontSetCombo;
end;
//------------------------------------------------------------------------------
//  Set the font info in the font combo boxes
//
procedure TTheMainForm.FontSetCombo;
var
  i : integer;
begin

  // Walk the combo box and find match

  for i := 0 to FontCombo.Items.Count - 1 do
    if CompareStr(FontCombo.Items[i], Self.Font.Name) = 0 then
      begin
        FontCombo.ItemIndex := i;
        break;
      end;

  FontComboHeight.Text := IntToStr(Self.Font.Size);
  FontComboHeight.Font.Name := Self.Font.Name;
end;
//------------------------------------------------------------------------------
// Save Font settings to INI-File
//
procedure TTheMainForm.FontIniSave (var Ini : TIniFile);
begin

  // Save everything about current font

  Ini.WriteString  ('Font', 'FontName',      Self.Font.Name);
  Ini.WriteInteger ('Font', 'FontSize',      Self.Font.Size);
  Ini.WriteString  ('Font', 'FontColor',     ColorToString(Self.Font.Color));
  Ini.WriteBool    ('Font', 'FontBold',      fsBold in Self.Font.Style);
  Ini.WriteBool    ('Font', 'FontItalic',    fsItalic in Self.Font.Style);
  Ini.WriteBool    ('Font', 'FontUnderline', fsUnderline in Self.Font.Style);
  Ini.WriteBool    ('Font', 'FontStrikeOut', fsStrikeOut in Self.Font.Style);

end;
//------------------------------------------------------------------------------
// Load Font settings from INI-File
//
procedure TTheMainForm.FontIniLoad (var Ini : TIniFile);
begin

  // Load everything about current font

  Self.Font.Name := Ini.ReadString ('Font', 'FontName', 'Arial');
  Self.Font.Size := Ini.ReadInteger('Font', 'FontSize', 10);
  Self.Font.Color := StringToColor (Ini.ReadString
                       ('Font', 'FontColor', ColorToString(clBlack)));

  if Ini.ReadBool ('Font', 'FontBold', false) then
    Self.Font.Style := Self.Font.Style + [fsBold];

  if Ini.ReadBool ('Font', 'FontItalic', false) then
    Self.Font.Style := Self.Font.Style + [fsItalic];

  if Ini.ReadBool ('Font', 'FontUnderline', false) then
    Self.Font.Style := Self.Font.Style + [fsUnderline];

  if Ini.ReadBool ('Font', 'FontStrikeOut', false) then
    Self.Font.Style := Self.Font.Style + [fsStrikeOut];

  FontSetCombo;
end;
//------------------------------------------------------------------------------
// Set font data for the FontCombo
//
procedure TTheMainForm.FontComboMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  if Control = FontCombo then
    if (Index >= 0) and (Index < FontCombo.Items.Count) then
    begin
      FontCombo.Canvas.Font.Name := FontCombo.Items[Index];
      FontCombo.Canvas.Font.Size := 10;
      Height := FontCombo.Canvas.TextHeight('Wg') + 2;
    end;
end;
//------------------------------------------------------------------------------
// Draw Font Combo Box
//
procedure TTheMainForm.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if Control = FontCombo then
    begin
      FontCombo.Canvas.FillRect(Rect);
      FontCombo.Canvas.Font.Name := FontCombo.Items[Index];
      FontCombo.Canvas.Font.Size := 10;
      FontCombo.Canvas.TextOut(Rect.Left+1, Rect.Top+1, FontCombo.Items[Index]);
    end;
end;
//------------------------------------------------------------------------------
// Action: ShowHint
//
procedure TTheMainForm.ActionSetHintOnOffExecute(Sender: TObject);
begin
  SetHint (not bHintOn);
end;
//------------------------------------------------------------------------------
// Set Showhint on or off
//
procedure TTheMainForm.SetHint (bHint : boolean);
begin
  bHintOn := bHint;
  ActionSetHintOnOff.Checked := bHintOn;
  Application.ShowHint := bHintOn;
end;
//------------------------------------------------------------------------------
// Set the right font in the hint window
//
procedure TTheMainForm.MyShowHint(var HintStr: string;
                var CanShow: Boolean; var HintInfo: THintInfo);
var
   i : integer;
begin

  for i := 0 to Application.ComponentCount - 1 do
     if Application.Components[i] is THintWindow then
      begin
        THintWindow(Application.Components[i]).Canvas.Font := self.Font;
        THintWindow(Application.Components[i]).Canvas.Font.Color := clRed;
        HintInfo.HintColor := $00F0F0F0; // Thin Thin Gray
      end;

 end;
//------------------------------------------------------------------------------
// Action: Set new Map directory
//
procedure TTheMainForm.ActionSetMapDirExecute(Sender: TObject);
begin
  MapSetDirDialog();
end;
//------------------------------------------------------------------------------
// Action: Save Map Data List
//
procedure TTheMainForm.ActionSaveMapExecute(Sender: TObject);
begin
  MapSaveToFile;
  GuiUpdateItemSelection;
end;
//------------------------------------------------------------------------------
// Action: Show About box
//
procedure TTheMainForm.ActionShowAboutExecute(Sender: TObject);
//var dlg : TAboutForm;
begin

  // Show the about dialog

  AboutDlg.Font := Font;
  AboutDlg.TabViews.Font := Font;
  AboutDlg.Show;

  // Place it where it should be

  AboutDlg.SetBounds(AboutDlgRect.Left,AboutDlgRect.Top,AboutDlgRect.Right,AboutDlgRect.Bottom);
  AboutDlg.TabViews.ActivePageIndex := AboutDlgActive;

end;
//------------------------------------------------------------------------------
// Action: Scale Full
//
procedure TTheMainForm.ActionScaleFullExecute(Sender: TObject);
begin
  Scale(0.02);
end;
//------------------------------------------------------------------------------
// Action: Scale 20, 40, 60, 80, 100, and 120%
//
procedure TTheMainForm.ActionScale20Execute(Sender: TObject);
begin
  Scale(0.2);
end;
procedure TTheMainForm.ActionScale40Execute(Sender: TObject);
begin
  Scale(0.4);
end;
procedure TTheMainForm.ActionScale60Execute(Sender: TObject);
begin
  Scale(0.6);
end;
procedure TTheMainForm.ActionScale80Execute(Sender: TObject);
begin
  Scale(0.8);
end;
procedure TTheMainForm.ActionScale100Execute(Sender: TObject);
begin
  Scale(1.0);
end;
procedure TTheMainForm.ActionScale120Execute(Sender: TObject);
begin
  Scale(1.2);
end;
procedure TTheMainForm.ActionScaleDownExecute(Sender: TObject);
begin
  Scale([drDown]);
end;
procedure TTheMainForm.ActionScaleUpExecute(Sender: TObject);
begin
  Scale([drUp]);
end;
//------------------------------------------------------------------------------
// Action: Set Scale Increment
//
procedure TTheMainForm.ActionScaleInc10Execute(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.SetScaleIncrement(10);
      GuiUpdateMapScale
    end
end;
procedure TTheMainForm.ActionScaleInc20Execute(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.SetScaleIncrement(20);
      GuiUpdateMapScale
    end
end;
procedure TTheMainForm.ActionScaleInc40Execute(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.SetScaleIncrement(40);
      GuiUpdateMapScale
    end
end;
//------------------------------------------------------------------------------
// Action: Manipulate Item
//
procedure TTheMainForm.ActionItemNewExecute(Sender: TObject);
var
  mp  : TMapItem;
  pnt : TPoint;
  it  : TMapItemType;
  gt  : TMapItemGeometry;
  sXy : string;
begin
  if (MapImageObj = nil) or
     (MapItemList = nil) or
     (MapItemTypeList = nil) then
    exit;

  // Create a new point with raw data

  // Find the middle of the map

  pnt := MapImageObj.CnvScrPntToMap(InqMapMid());

  // Get the item type to use
  
  it := MapItemTypeList.GetDefaultType();
  gt := it.InqGeometry;
  case gt of
  gtLine, gtCubic  : sXy := IntToStr(pnt.X - 20) + ',' + IntToStr(pnt.Y - 20) + ',' +
                   IntToStr(pnt.X + 10) + ',' + IntToStr(pnt.Y - 10) + ',' +
                   IntToStr(pnt.X - 10) + ',' + IntToStr(pnt.Y + 10) + ',' +
                   IntToStr(pnt.X + 20) + ',' + IntToStr(pnt.Y + 20) + ',';
  gtArea  : sXy := IntToStr(pnt.X) + ',' + IntToStr(pnt.Y - 30) + ',' +
                   IntToStr(pnt.X + 30) + ',' + IntToStr(pnt.Y) + ',' +
                   IntToStr(pnt.X) + ',' + IntToStr(pnt.Y + 30) + ',' +
                   IntToStr(pnt.X - 30) + ',' + IntToStr(pnt.Y) + ',';
  gtPoint : sXy := IntToStr(pnt.X) + ',' + IntToStr(pnt.Y) + ',';
  end;

  mp := TMapItem.Create(MapItemTypeList, 'New Item',it.InqName(),gt,sXy,30,'No description');

  // Make sure the name is unique

  mp.SetName( MapItemList.GetUniqueName('New Item'));

  // Add it to MapItemList

  MapItemList.ItemNew(mp,udNewItem);

  // Add it in the listbox

  MapItemListBoxAdd(mp);

  // Set focus on it

  MapItemSelect (mp);
end;
//------------------------------------------------------------------------------
// Copy current item
//
procedure TTheMainForm.ActionItemCopyExecute(Sender: TObject);
begin
  if MapItemCur <> nil then
    MapItemCopy := MapItemCur;
  GuiUpdateItemSelection;
end;
procedure TTheMainForm.ActionItemPasteExecute(Sender: TObject);
var
  newP     : TMapItem;
  pMid     : TPoint;
  pItem    : TPoint;
begin

  // Paste the point copied

  if MapItemCopy <> nil then
    begin
      newP := MapItemCopy.Copy();
      if newP <> nil then
        begin

          // Place the new item in middle of the screen

          pMid := MapImageObj.CnvScrPntToMap(InqMapMid());
          pItem := newP.InqMidPoint();
          newP.Move(PntSubMove(pMid, pItem));

          // Make sure the new items name is unique

          newP.SetName( MapItemList.GetUniqueName(newP.InqName()));

          // Add it to map data

          MapItemList.ItemNew(newP, udPasteItem);

          // Add it in the listbox

          MapItemListBoxAdd(newP);

          // Set focus on it

          MapItemSelect (newP);
        end;
    end;
end;
procedure TTheMainForm.ActionItemDelExecute(Sender: TObject);
var
  s : string;
  r : integer;
begin
  // Delete the item in the listbox

  if MapItemCur <> nil then
    begin
      s := 'Do you really want to delete ' + MapItemCur.InqName + '?';
      r := Application.MessageBox(PAnsiChar(s),
                    'On delete item',MB_YESNO);
      if (r = 0) or (r = IDYES) then
        begin

          // Make sure that any copied item that is deleted dont get copied

          if (MapItemCopy = MapItemCur) then
            MapItemCopy := nil;

          // Remove from listbox first

          MapItemListBoxDel(MapItemCur);

          // Remove item

          MapItemList.ItemDel(MapItemCur, udDelItem);
          MapItemSelect (nil);
        end;
    end;
end;
procedure TTheMainForm.ActionItemSetInMidExecute(Sender: TObject);
var
  pTo, pFrom : TPoint;
begin
  if MapItemCur <> nil then
    begin
      pTo := MapImageObj.CnvScrPntToMap(InqMapMid());
      pFrom := MapItemCur.InqMidPoint();
      pTo := PntSubMove (pTo, pFrom);

      MapItemList.ItemMove(MapItemCur,pTo,udMoveMid);

      GuiUpdateItemSelection;
    end;
end;
procedure TTheMainForm.ActionShowNoTypesExecute(Sender: TObject);
begin
  MapItemTypeList.SetVisibleAll(false);
  MapItemListBoxLoad;
  GuiUpdateItemSelection
end;

procedure TTheMainForm.ActionShowAllTypesExecute(Sender: TObject);
begin
  MapItemTypeList.SetVisibleAll(true);
  MapItemListBoxLoad;
  GuiUpdateItemSelection
end;
//------------------------------------------------------------------------------
//                    Form rezising (incl splitter)
//------------------------------------------------------------------------------
// Calculate all Resizing of controls in the Main Form
//
//    iType = 0 Just adjust all controls as they are (redraw)
//    iType = 1 Make sure aspects ratios are kept (resize mainform)
//    iType = 2 Make sure MapPanel are splittered (move splitter)
//    iType = 3 Make sure ListPanel are splittered (move splitter)
//    iType = 4 Make sure DescPanel are splittered (move splitter)
//
procedure TTheMainForm.GuiUpdateControls (iType : integer);
const ScrollButtonSize  = 16; // Size of the scroll buttons
const SplitterClearance =  8; // Size of splitter bars
const SmallBorder       =  2; // Size of some really tiny borders
var
  wdt : integer;
begin

  // Set invisble and start timer to make it visible later

  Map.Visible := false;
  MapTimer.Enabled := true;

  //--- Set size of the Status Bar ---------------------------------------------

  StatusBar.Height := round(abs(Font.Height) * 1.5); // Use current font
  wdt := Self.Canvas.TextWidth ('XXXXX');

  StatusBar.Panels[StatusApp].Width   := ClientWidth - wdt * 11;
  StatusBar.Panels[StatusMap].Width   := wdt * 4;
  StatusBar.Panels[StatusItem].Width  := wdt * 4;
  StatusBar.Panels[StatusScale].Width := wdt * 1;
  // Make sure the last has 2 * wdt left

  //--- Set the BasePanel and DescPanel sizes ----------------------------------

  if iType = 4 then
    begin
      // Make sure the DescPanel Splitter is used as base

      MapItemDesc.Top := DescSplitter.Top + SplitterClearance;

      // Remember this height of DescPanel

      globSplitterDescHeight := TheMainForm.ClientHeight - MapItemDesc.Top;
    end
  else if (iType = 1) then
    begin
      // Make sure to keep height on DescPanel

      MapItemDesc.Top := TheMainForm.ClientHeight - globSplitterDescHeight;
      if CoolBar.Visible then
        BasePanel.Height := MapItemDesc.Top - SplitterClearance - CoolBar.Height
      else
        BasePanel.Height := MapItemDesc.Top - SplitterClearance;
    end;

  MapItemDesc.Height := TheMainForm.ClientHeight - MapItemDesc.Top - StatusBar.Height;
  MapItemDesc.Left := 0;
  MapItemDesc.Width := TheMainForm.ClientWidth;

  //--- Set the Listboxes inside ListPanel -------------------------------------

  if iType = 3 then
    begin
      // Remember this height of DescPanel

      globSplitterListHeight := MapListBox.Height;
    end
  else if (iType = 1) then
    begin
      // Make sure to keep height on MapListBox

      MapListBox.Height := globSplitterListHeight;
    end;

  MapItemListBox.Top := ListSplitter.Top + SplitterClearance;
  MapItemListBox.Height := ListPanel.Height - MapItemListBox.Top;// - SmallBorder * 2;
  MapItemListBox.Left := 0; //SmallBorder;
  MapItemListBox.Width := ListPanel.Width; //- SmallBorder * 2;

  //--- Set the Map Panel and its content --------------------------------------

  if iType = 2 then
    begin
      // Remember this height of DescPanel

      globSplitterListWidth := ListPanel.Width;
    end
  else if (iType = 1) then
    begin
      // Make sure to keep width on Listpanel

      ListPanel.Width := globSplitterListWidth;
    end;

  MapPanel.Top := 0;
  MapPanel.Height := BasePanel.Height;
  MapPanel.Left := MapSplitter.Left + SplitterClearance;
  MapPanel.Width := BasePanel.ClientWidth - MapPanel.Left - SmallBorder * 2;

  //--- Set Map inside MapPanel ------------------------------------------------

  Map.Left  := ScrollButtonSize;
  Map.Width := MapPanel.ClientWidth - ScrollButtonSize * 2;
  Map.Top := ScrollButtonSize;
  Map.Height := MapPanel.ClientHeight - ScrollButtonSize * 2;

  ScrlLeftUp.Top := 0;
  ScrlLeftUp.Height := ScrollButtonSize;
  ScrlLeftUp.Left := 0;
  ScrlLeftUp.Width := ScrollButtonSize;

  ScrlLeft.Top := ScrollButtonSize;
  ScrlLeft.Height := MapPanel.ClientHeight - ScrollButtonSize * 2;
  ScrlLeft.Left := 0;
  ScrlLeft.Width := ScrollButtonSize;

  ScrlLeftDown.Top := MapPanel.ClientHeight - ScrollButtonSize;
  ScrlLeftDown.Height := ScrollButtonSize;
  ScrlLeftDown.Left := 0;
  ScrlLeftDown.Width := ScrollButtonSize;

  ScrlDown.Top := ScrlLeftDown.Top;
  ScrlDown.Height := ScrollButtonSize;
  ScrlDown.Left := ScrollButtonSize;
  ScrlDown.Width := MapPanel.ClientWidth - ScrollButtonSize * 2;

  ScrlRightDown.Top := ScrlLeftDown.Top;
  ScrlRightDown.Height := ScrollButtonSize;
  ScrlRightDown.Left := MapPanel.ClientWidth - ScrollButtonSize;
  ScrlRightDown.Width := ScrollButtonSize;

  ScrlRight.Top := ScrollButtonSize;
  ScrlRight.Height := MapPanel.ClientHeight - ScrollButtonSize * 2;
  ScrlRight.Left := MapPanel.ClientWidth - ScrollButtonSize;
  ScrlRight.Width := ScrollButtonSize;

  ScrlRightUp.Top := 0;
  ScrlRightUp.Height := ScrollButtonSize;
  ScrlRightUp.Left := MapPanel.ClientWidth - ScrollButtonSize;
  ScrlRightUp.Width := ScrollButtonSize;

  ScrlUp.Top := 0;
  ScrlUp.Height := ScrollButtonSize;
  ScrlUp.Left := ScrollButtonSize;
  ScrlUp.Width := MapPanel.ClientWidth - ScrollButtonSize * 2;

end;
//------------------------------------------------------------------------------
// Set the map visible again after a resize job
//
procedure TTheMainForm.MapTimerTimer(Sender: TObject);
begin
  if not Map.Visible then
    begin

      // Set the map visible

      Map.Visible := true;
      MapTimer.Enabled := false;

      // Make sure current scale is ok (not too small)

      MapResizeFirst := true;
      MapImageObj.ScaleAssure(MapResizeMapPos);
      GuiUpdateMapScale;
      GuiUpdateItemSelection;
    end
  else
    MapTimer.Enabled := false;

end;
//------------------------------------------------------------------------------
// User has resized the Form
//
procedure TTheMainForm.FormResize(Sender: TObject);
begin
  if not (AppStatus = asOnRunning) then
    GuiUpdateControls (1); // Always call the BIG function to handle everything
end;
//------------------------------------------------------------------------------
// User is moving the Splitter between ListPanel and MapPanel
//
procedure TTheMainForm.MapSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := not ((NewSize < 100) or (NewSize > TheMainForm.ClientWidth / 2));
end;
//------------------------------------------------------------------------------
// User has moved the Splitter between ListPanel and MapPanel
//
procedure TTheMainForm.MapSplitterMoved(Sender: TObject);
begin
//  SplitterPanel.Width := MapSplitter.Left;
  GuiUpdateControls (2);
end;
//------------------------------------------------------------------------------
// User is moving the Splitter between ListPanel and MapPanel
// Allowe a min Maplistbox height of 60 and half of the ListPanels heigth
//
procedure TTheMainForm.ListSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := not ((NewSize < 60) or
                 (NewSize > (ListPanel.Height - 60)));
end;
//------------------------------------------------------------------------------
// User has moved the Splitter between ListPanel and MapPanel
//
procedure TTheMainForm.ListSplitterMoved(Sender: TObject);
begin
  GuiUpdateControls(3);
end;
//------------------------------------------------------------------------------
// User is moving the Splitter between upper Map list and lower Item list
// Allow only moving up to half widow size, down to two rows
//
procedure TTheMainForm.DescSplitterCanResize(Sender: TObject;
                      var NewSize: Integer; var Accept: Boolean);
begin
  Accept := ((NewSize > (0.5 * TheMainForm.ClientHeight)) and
     (NewSize < (TheMainForm.ClientHeight - (StatusBar.Height * 2) - 12)));
end;
//------------------------------------------------------------------------------
// User has moved the Splitter between ListPanel and MapPanel
//
procedure TTheMainForm.DescSplitterMoved(Sender: TObject);
begin
 GuiUpdateControls(4);
end;
//------------------------------------------------------------------------------
//                           Point List sorting
//------------------------------------------------------------------------------
// Point List sorting
//
procedure TTheMainForm.MapItemListBoxColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ColumnToSortItems := Column.Index;
  (Sender as TCustomListView).AlphaSort;

  // Reverese sort order next time

  if ColumnToSortItems = 0 then
    ColumnToSortItemsNameDown := not ColumnToSortItemsNameDown
  else
    ColumnToSortItemsTypeDown := not ColumnToSortItemsTypeDown;

  ColumnToSortItems := 0;
end;
//------------------------------------------------------------------------------
// Point List Sorting Compare function
//
procedure TTheMainForm.MapItemListBoxCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
begin

  // Decide which column to sort

  if ColumnToSortItems = 0 then
    begin

      if ColumnToSortItemsNameDown then
        Compare := CompareText(Item1.Caption,Item2.Caption)
      else
        Compare := - CompareText(Item1.Caption,Item2.Caption)
    end
  else if (Item1.SubItems.Count >= ColumnToSortItems) then
    begin
      ix := ColumnToSortItems - 1;
      if ColumnToSortItemsTypeDown then
        Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix])
      else
        Compare := - CompareText(Item1.SubItems[ix],Item2.SubItems[ix]);

    end;
    
  // Start the Map item timer in order to make the current item selected again

  MapItemTimer.Enabled := true;
end;
//------------------------------------------------------------------------------
// Map List Sorting
//
procedure TTheMainForm.MapListBoxColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ColumnToSortMaps := Column.Index;
  (Sender as TCustomListView).AlphaSort;

  // Reverese sort order next time

  if ColumnToSortMaps = 0 then
    ColumnToSortMapsNameDown := not ColumnToSortMapsNameDown
  else
    ColumnToSortMapsDescDown := not ColumnToSortMapsDescDown;

  ColumnToSortMaps := 0;
end;
//------------------------------------------------------------------------------
// Map List Sorting Compare
//
procedure TTheMainForm.MapListBoxCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
begin

  // Decide which column to sort

  if ColumnToSortMaps = 0 then
    begin

      if ColumnToSortMapsNameDown then
        Compare := CompareText(Item1.Caption,Item2.Caption)
      else
        Compare := - CompareText(Item1.Caption,Item2.Caption)
    end
  else if (Item1.SubItems.Count >= ColumnToSortMaps) then
    begin
      ix := ColumnToSortMaps - 1;
      if ColumnToSortMapsDescDown then
        Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix])
      else
        Compare := - CompareText(Item1.SubItems[ix],Item2.SubItems[ix]);
    end;
end;
//------------------------------------------------------------------------------
//                              Scrolling Map
//------------------------------------------------------------------------------
//
//
procedure TTheMainForm.ScrlLeftUpClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drLeft] + [drUp]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end;
end;
procedure TTheMainForm.ScrlLeftClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drLeft]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end;
procedure TTheMainForm.ScrlLeftDownClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drLeft] + [drDown]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end;
procedure TTheMainForm.ScrlDownClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drDown]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end;
procedure TTheMainForm.ScrlRightDownClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drRight] + [drDown]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end;
procedure TTheMainForm.ScrlRightClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drRight]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end;
procedure TTheMainForm.ScrlRightUpClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drRight] + [drUp]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end;
procedure TTheMainForm.ScrlUpClick(Sender: TObject);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Move ([drUp]);
      GuiUpdateMapScroll;
      GuiUpdateItemSelection
    end
end; 
//------------------------------------------------------------------------------
// Set which Scroll button schould be enabled
//
procedure TTheMainForm.GuiUpdateMapScroll;
var
  dirOpt : TDirections;
begin
  if MapImageObj <> nil then
    begin
      dirOpt := MapImageObj.InqMoveOptions;

      ScrlLeftUp.Enabled    := (drLeft in dirOpt) or (drUp in dirOpt);
      ScrlLeft.Enabled      := drLeft in dirOpt;
      ScrlLeftDown.Enabled  := (drLeft in dirOpt) or (drDown in dirOpt);
      ScrlDown.Enabled      := drDown in dirOpt;
      ScrlRightDown.Enabled := (drRight in dirOpt) or (drDown in dirOpt);
      ScrlRight.Enabled     := drRight in dirOpt;
      ScrlRightUp.Enabled   := (drRight in dirOpt) or (drUp in dirOpt);
      ScrlUp.Enabled        := drUp in dirOpt;
    end
  else
    begin
      ScrlLeftUp.Enabled    := false;
      ScrlLeft.Enabled      := false;
      ScrlLeftDown.Enabled  := false;
      ScrlDown.Enabled      := false;
      ScrlRightDown.Enabled := false;
      ScrlRight.Enabled     := false;
      ScrlRightUp.Enabled   := false;
      ScrlUp.Enabled        := false;
    end;
end;
//------------------------------------------------------------------------------
//                              Scaling Map
//------------------------------------------------------------------------------
// Scale up/down with center of the map still in center
//
procedure TTheMainForm.Scale (dir : TDirections);
begin
  if MapImageObj <> nil then
    begin
      MapImageObj.Scale(dir, InqMapMid());
      GuiUpdateMapScale;
      GuiUpdateItemSelection
    end
end;
//------------------------------------------------------------------------------
// Scale to a fix scale with center at a specific place
//
procedure TTheMainForm.Scale (scl : real; pScr : TPoint);
var
  pMap : TPoint;
begin
  if MapImageObj <> nil then
    begin
      pMap := MapImageObj.CnvScrPntToMap(pScr);
      MapImageObj.Scale(scl);
      MapImageObj.MoveMapPntToScrPnt(pMap,pScr);
      GuiUpdateMapScale;
      GuiUpdateItemSelection
    end
end;
//------------------------------------------------------------------------------
// Scale to a fix scale with center of the map still in center
//
procedure TTheMainForm.Scale (scl : real);
var
  pMap : TPoint;
begin
  if MapImageObj <> nil then
    begin
      pMap := MapImageObj.CnvScrPntToMap(InqMapMid());
      MapImageObj.Scale(scl);
      MapImageObj.MoveMapPntToScrPnt(pMap,InqMapMid());
      GuiUpdateMapScale;
      GuiUpdateItemSelection
    end
end;
//------------------------------------------------------------------------------
// Set which Scale button schould be enabled
//
procedure TTheMainForm.GuiUpdateMapScale;
var
  curScl   : real;
  minScl   : real;
  bSclUp   : boolean;
  bSclDown : boolean;
begin
  if MapImageObj <> nil then
    begin
      curScl   := round(10.0 * MapImageObj.InqScale());
      minScl   := MapImageObj.InqScaleMin();
      bSclUp   := drUp in MapImageObj.InqScaleOptions();
      bSclDown := drDown in MapImageObj.InqScaleOptions();

      ActionScaleUp.Enabled   := bSclUp;
      ActionScaleDown.Enabled := bSclDown;

      ActionScaleFull.Enabled := bSclDown;
      ActionScale20.Enabled   := (minScl <= 0.2) and (curScl <> 2);
      ActionScale40.Enabled   := (minScl <= 0.4) and (curScl <> 4);
      ActionScale60.Enabled   := (minScl <= 0.6) and (curScl <> 6);
      ActionScale80.Enabled   := (minScl <= 0.8) and (curScl <> 8);
      ActionScale100.Enabled  := (minScl <= 1.0) and (curScl <> 10);
      ActionScale120.Enabled  := (minScl <= 1.2) and (curScl <> 12);

      ActionScaleInc10.Checked := MapImageObj.InqScaleIncrement() = 10;
      ActionScaleInc20.Checked := MapImageObj.InqScaleIncrement() = 20;
      ActionScaleInc40.Checked := MapImageObj.InqScaleIncrement() = 40;

      StatusBar.Panels[StatusScale].Text := FloatToStr(round(100 *
                  MapImageObj.InqScale())) + '%';
    end
  else
    begin
      ActionScaleUp.Enabled   := false;
      ActionScaleDown.Enabled := false;

      ActionScaleFull.Enabled := false;
      ActionScale20.Enabled :=  false;
      ActionScale40.Enabled :=  false;
      ActionScale60.Enabled :=  false;
      ActionScale80.Enabled :=  false;
      ActionScale100.Enabled :=  false;
      ActionScale120.Enabled :=  false;

      ActionScaleInc10.Checked := false;
      ActionScaleInc20.Checked := false;
      ActionScaleInc40.Checked := false;

      StatusBar.Panels[StatusScale].Text := 'N/A';
    end
end;
//------------------------------------------------------------------------------
//                               Edit Mode
//
procedure TTheMainForm.ActionSetEditModeExecute(Sender: TObject);
begin
  EditMode := not EditMode;
  GuiUpdateItemSelection
end;
//------------------------------------------------------------------------------
//  Update if user changed Edit Mode
//
procedure TTheMainForm.GuiUpdateItemSelection;
var
  bSave : boolean; // True id map is dirty
  i : integer;
begin
  bSave := false;

  if (MapImageObj <> nil) and (MapItemList <> nil) then
    begin

      MapImageObj.BaseDraw;

      if HighlightAllItems then
        begin
          for i := 0 to MapItemList.InqPoints() -1 do
            MapItemList.InqPoint(i).Draw(MapImageObj, false, false,1);
        end;

      if (MapItemCur <> nil) and (not DrawTimer.Enabled) then
        begin

          // Draw the item

          MapItemCur.Draw(MapImageObj, EditMode);

          // Draw an edit frame

          if EditMode then
            MapImageObj.DrawEditFrame(MapItemCur.InqExt, MapItemCur.InqPointLen() > 1);
        end;

      // Test if the map is dirty (can be saved)

      bSave := MapItemList.InqDirty();
    end;

  // Do actions that rely on Edit Mode and curren item selection

  ActionSetEditMode.Checked    := EditMode and (MapItemCur <> nil);
  ActionItemCopy.Enabled       := EditMode and (MapItemCur <> nil);
  ActionItemDel.Enabled        := EditMode and (MapItemCur <> nil);
  ActionItemSetInMid.Enabled   := EditMode and (MapItemCur <> nil);
  ActionSetNewItemType.Enabled := EditMode and (MapItemCur <> nil);
  ActionSetItemColor.Enabled   := EditMode and (MapItemCur <> nil);
  SetItemTypeSub.Enabled       := EditMode and (MapItemCur <> nil);;

  // Do actions that rely on anything copied

  ActionItemPaste.Enabled    := EditMode and (MapItemCopy <> nil);

  // Do the actions that rely on dirt map

  ActionSaveMap.Enabled := bSave;

  // Do all actions that only rely on Edit Mode

  if EditModeLast <> EditMode then
    begin
      MapItemListBox.ReadOnly    := not EditMode;
      MapItemDesc.ReadOnly       := not EditMode;
      MapItemListBox.ReadOnly    := not EditMode;

      ActionItemNew.Enabled      := EditMode;

      if EditMode then
        begin
          MapListBox.Color     := clWindow;
          MapItemListBox.Color := clWindow;
          MapItemDesc.Color    := clWindow;
        end
      else
        begin
          MapListBox.Color     := $00E5E5E5;
          MapItemListBox.Color := $00E5E5E5;
          MapItemDesc.Color    := $00E5E5E5;
        end;
    end;

  // Do the actions that rely on undo

  if MapItemList <> nil then
    begin
      ActionUndo.Enabled  := EditMode and (MapItemList.InqUndoLen > 0);
      ToolButtonUndo.Hint := MapItemList.InqNextUndoInfo();
    end
  else
    begin
      ActionUndo.Enabled  := false;
      ToolButtonUndo.Hint := 'No items loaded';
    end;

  // Do other misc actions

  ActionShowAllItems.Checked := HighlightAllItems;
  ActionShowNifty.Checked    := DrawNiftyOn;

  // Set the current Update modes for next Update

  EditModeLast := EditMode;  // Last edit mode in Update Gui

end;
//------------------------------------------------------------------------------
//  Update the menues for Item Types
//
procedure TTheMainForm.GuiUpdateMapItemTypes;
var
  i, j, pos : integer;
  mItem : TMenuItem;
  it    : TMapItemType;
  found : boolean;
begin
  if MapItemTypeList = nil then exit;

  //--- Begin with Visibility (View1 and View2) -------------------------

  // Walk all item types

  for i := 0 to MapItemTypeList.Count -1 do
    begin
      it := MapItemTypeList.Items[i];
      if it <> nil then
        begin
          // Do View1 : Is there a menu for this item

          found := false;
          j := 0;
          while (not found) and (j < View1.Count) do
            begin
              mItem := View1.Items[j];
              if mItem <> nil then
                if strComp(PAnsiChar(mItem.Caption),PAnsiChar(it.InqName())) = 0 then
                  found := true;
              j := j + 1;
            end;

          if not found then
            begin
              // Add this item as a new menu item also

              mItem := TMenuItem.Create(self);
              mItem.Caption := it.InqName();
              mItem.Checked := true;
              mItem.OnClick := SetItemTypeVisibility;
              pos := View1.IndexOf(DrawNone);
              View1.Insert(pos + 1,mItem);
            end;

          // Do the SetItemTypeSub menu also

          found := false;
          j := 0;
          while (not found) and (j < SetItemTypeSub.Count) do
            begin
              mItem := SetItemTypeSub.Items[j];
              if mItem <> nil then
                if strComp(PAnsiChar(mItem.Caption),PAnsiChar(it.InqName())) = 0 then
                  found := true;
              j := j + 1;
            end;

          if not found then
            begin
              // Add this item as a new menu item also

              mItem := TMenuItem.Create(self);
              mItem.Caption := it.InqName();
              mItem.OnClick := SetItemTypeFromMenu;
              pos := SetItemTypeSub.IndexOf(SetNewItemType);
              SetItemTypeSub.Insert(pos + 1,mItem);
            end;

          // Do the SetItemTypeSub menu also

          found := false;
          j := 0;
          while (not found) and (j < MenuItemType.Count) do
            begin
              mItem := MenuItemType.Items[j];
              if mItem <> nil then
                if strComp(PAnsiChar(mItem.Caption),PAnsiChar(it.InqName())) = 0 then
                  found := true;
              j := j + 1;
            end;

          if not found then
            begin
              // Add this item as a new menu item also

              mItem := TMenuItem.Create(self);
              mItem.Caption := it.InqName();
              mItem.OnClick := SetItemTypeFromMenu;
              pos := MenuItemType.IndexOf(NewTypeSpace);
              MenuItemType.Insert(pos + 1,mItem);
            end;
         end;

    end;

end;
//------------------------------------------------------------------------------
//  Clear all old item types in the menu
//
procedure TTheMainForm.ClearAllItemTypesInMenus;
var
  first, i, last : integer;
  mi : TMenuItem;
begin

  // Do View1

  first := View1.IndexOf(DrawNone) + 1;
  last  := View1.IndexOf(DrawAll) - 1;

  for i := last downto first do
    begin
      mi := View1.Items[i];
      if mi <> nil then
        begin
          View1.Delete(i);
          mi.Free;
        end;
    end;

  // Do SetItemTypeSub

  first := SetItemTypeSub.IndexOf(SetNewItemType) + 1;
  last  := SetItemTypeSub.Count - 1;

  for i := last downto first do
    begin
      mi := SetItemTypeSub.Items[i];
      if mi <> nil then
        begin
          SetItemTypeSub.Delete(i);
          mi.Free;
        end;
    end;
    

  // Do SetItemTypeSub

  first := MenuItemType.IndexOf(NewTypeSpace) + 1;
  last  := MenuItemType.Count - 1;

  for i := last downto first do
    begin
      mi := MenuItemType.Items[i];
      if mi <> nil then
        begin
          MenuItemType.Delete(i);
          mi.Free;
        end;
    end;

end;
//------------------------------------------------------------------------------
//  Handle User clicked on a Item Type Show Menu (Handles all menues)
//
procedure TTheMainForm.SetItemTypeVisibility(Sender: TObject);
var
  first, i, last : integer;
  it : TMapItemType;
  mi : TMenuItem;
  found : boolean;

begin
  if MapItemTypeList = nil then exit;

  found := false;

  With Sender as TMenuItem do
    begin

      // Find the Item Type

      it := MapItemTypeList.GetItemType (Caption);
      if it <> nil then
        begin
          // Toggle the visibility

          it.SetVisible(not it.InqVisible());
          found := true;
        end;
    end;

  if not found then exit;

  //--- Walk all menues to set it right  ------

  // Do View1

  first := View1.IndexOf(DrawNone) + 1;
  last  := View1.IndexOf(DrawAll) - 1;

  for i := last downto first do
    begin
      mi := View1.Items[i];
      if mi <> nil then
        begin
          it := MapItemTypeList.GetItemType (mi.Caption);
          if it <> nil then
             mi.Checked := it.InqVisible();
        end;
    end;

  // Redraw listbox also

  MapItemListBoxLoad;

end;
//------------------------------------------------------------------------------
//  Handle User clicked on a Item Type Set Menu (Handles all menues)
//
procedure TTheMainForm.SetItemTypeFromMenu(Sender: TObject);
var
  it : TMapItemType;
begin
  if MapItemTypeList = nil then exit;

  With Sender as TMenuItem do
    begin
      // Find the Item Type, and set it on selected item in list box

      it := MapItemTypeList.GetItemType (Caption);
      if it <> nil then
        SetItemType(it);
    end;
end;
//------------------------------------------------------------------------------
// Set a new item type
//
procedure TTheMainForm.SetItemType(it : TMapItemType);
var
  i : integer;
begin
  if MapItemTypeList = nil then exit;

  // Get the MapDataPont of the selection in the Map Point Listbox

  if (MapItemCur <> nil) and (it <> nil) then
    begin

      // Set this new point type

      MapItemCur.SetType(MapItemTypeList, it);

      // Set this as default also

      MapItemTypeList.SetDefaultType(it);

      // Update the type name in listbox

      for i := 0 to MapItemListBox.Items.Count - 1do
        if MapItemListBox.Items[i].Data = MapItemCur then
          MapItemListBox.Items[i].SubItems[0] := MapItemCur.InqTypeName;

      // Redraw

      GuiUpdateItemSelection;
    end;
end;
//------------------------------------------------------------------------------
// Open dialog to create a new item type
//
procedure TTheMainForm.ActionSetNewItemTypeExecute(Sender: TObject);
var
  dlg    : TMapItemTypeNewForm;
  itName : string;
  gtName : string;
  sTmp   : string;
  i      : integer;
  it     : TMapItemType;
  gt     : TMapItemGeometry;
begin
  if MapItemTypeList = nil then exit;

  // Open the dialog and create a new item type and use it also

  dlg := TMapItemTypeNewForm.Create(self);
  dlg.Font := Font;

  // Load the listbox

  for i := MapItemLastMin to MapItemLastMax do
    begin
      if length(MapItemTypesLastUsed[i].TypeName) > 0 then
        {if MapItemTypeList.GetItemType(MapItemTypesLastUsed[i].TypeName) = nil then }
        begin

          sTmp := MapItemTypesLastUsed[i].TypeName + ' (' +
                  MapItemTypesLastUsed[i].TypeGeom + ')';
          if length(sTmp) > 0 then
            dlg.ItemListBox.AddItem(sTmp, nil);
        end;
    end;

  // Set the cuurent Map type

  {if MapItemCur <> nil then
    begin
      dlg.NewItemType.Text := MapItemCur.InqTypeName();
      case MapItemCur.InqGeometry() of
        gtPoint: dlg.GeomPoint.Checked := true;
        gtLine : dlg.GeomLine.Checked := true;
        gtCubic: dlg.GeomCubic.Checked := true;
        gtArea : dlg.GeomArea.Checked := true;
      end;
    end; }

  dlg.ShowModal;

  // get the result

  if dlg.InqNew() then
    begin
      // Test if this new Item type exist already

      itName := dlg.InqTypeName();
      it := MapItemTypeList.GetItemType(itName);
      if (it = nil) and (length(itName) > 0) then
        begin

          // Translate to geometry type

          gtName := dlg.InqGeometryName();
          case gtName[1] of
          'A': gt := gtArea;
          'L': gt := gtLine;
          'C': gt := gtCubic;
          else
            gt := gtPoint;
          end;

          // Add this new item type

          it := MapItemTypeList.Add(itName, gt);
          if (it <> nil) then
            begin
              // Set the new type on current item also

              SetItemType(it);

              // Add it to the last used

              IniSaveLastItemsAdd (it);
            end;
          GuiUpdateMapItemTypes;
        end;
    end;

  dlg.Release;
end;
//------------------------------------------------------------------------------
//                               CoolBar Stuff
//------------------------------------------------------------------------------
// Toggle Coolbar on/off
//
procedure TTheMainForm.ActionToolbarToggleExecute(Sender: TObject);
begin
  ActionToolbarToggle.Checked := not ActionToolbarToggle.Checked;
  CoolBar.Visible := ActionToolbarToggle.Checked;
  GuiUpdateControls (1);
end;
//------------------------------------------------------------------------------
//  Initiate CoolBars
//
procedure TTheMainForm.CoolBarInitGui;
var
  i, n, w : integer;
  mItem   : TMenuItem;
begin
  n := 0;

  // Walk all ToolBars in The CoolBar.
  // All ToolBars are docked there from the start of the application
  // Later they can be floating

  for i := 0 to CoolBar.Bands.Count -1 do
    begin
      if CoolBar.Bands[i].Control <> nil then
        begin
          // Add this as a menubar

          mItem := TMenuItem.Create(self);

          if CoolBar.Bands[i].Control = ToolBarFile then
            mItem.Caption := 'File Band'
          else if CoolBar.Bands[i].Control = ToolBarEdit then
            mItem.Caption := 'Edit Band'
          else if CoolBar.Bands[i].Control = ToolBarView then
            mItem.Caption := 'View Band'
          else if CoolBar.Bands[i].Control = ToolBarScale then
            mItem.Caption := 'Scale Band'
          else if CoolBar.Bands[i].Control = ToolBarFont then
            mItem.Caption := 'Font Band';

          mItem.Checked := CoolBar.Bands[i].Visible;
          mItem.OnClick := CoolBarMenuCallBack;
          ToolBarButtons.Insert(n, mItem);
          n := n + 1;
        end
    end;

  // Walk All CoolBands and set Min Width

  for i := 0 to CoolBar.Bands.Count -1 do
    begin
      if CoolBar.Bands[i].Control <> nil then
        begin
          w := 1;

          for n := 0 to CoolBar.Bands[i].Control.ControlCount - 1 do
            w := w + CoolBar.Bands[i].Control.Controls[n].Width;
          CoolBar.Bands[i].MinWidth := w;
        end;
    end;

  //ToolBarScale.FloatingDockSiteClass := TDockForm;
end;
//------------------------------------------------------------------------------
//  Toggle this Toolbar button on/off
//
procedure TTheMainForm.CoolBarMenuCallBack(Sender: TObject);
var
  i     : integer;
  mItem : TMenuItem;
  Cntrl : TControl;
begin
  if Sender.ClassType <> TMenuItem then exit;

  mItem := TMenuItem(Sender);

  // COnvert Manu Caption to ToolBar Control pointer

  if (CompareStr(mItem.Caption, 'File Band') = 0) then
    Cntrl := ToolBarFile
  else if (CompareStr(mItem.Caption, 'Edit Band') = 0) then
    Cntrl := ToolBarEdit
  else if (CompareStr(mItem.Caption, 'View Band') = 0) then
    Cntrl := ToolBarView
  else if (CompareStr(mItem.Caption, 'Edit Band') = 0) then
    Cntrl := ToolBarFile
  else if (CompareStr(mItem.Caption, 'Scale Band') = 0) then
    Cntrl := ToolBarScale
  else if (CompareStr(mItem.Caption, 'Font Band') = 0) then
    Cntrl := ToolBarFont
  else
    Exit;

  // Test if its docked to the CoolBar

  if Cntrl.HostDockSite = CoolBar then
    begin
      // Find the band it is docked to

      for i := 0 to CoolBar.Bands.Count -1 do
        if CoolBar.Bands[i].Control = Cntrl then
          begin
            // Its docked to this band, toggle the visibility

            CoolBar.Bands[i].Visible := not CoolBar.Bands[i].Visible;
            mItem.Checked :=  CoolBar.Bands[i].Visible;
          end
    end
  else
    begin

      // Still here, toggle the controls visibility instead

      Cntrl.Visible := not Cntrl.Visible;
      mItem.Checked :=  Cntrl.Visible;
    end
end;
//------------------------------------------------------------------------------
//  Set the width of all floating toolbars
//
procedure TTheMainForm.ToolBarScaleEndDock(Sender, Target: TObject; X,
  Y: Integer);
var
  i,w : integer;
begin
  w := 0;
  for i := 0 to ToolBarScale.ControlCount -1 do
    w := w + ToolBarScale.Controls[i].Width;

  ToolBarScale.Width := w ;
  StatusBar.Panels[0].Text := 'ToolBar Scale OnEndDock to: ' +
  ToolBarScale.HostDockSite.Owner.ClassName;
end;
procedure TTheMainForm.ToolBarFontEndDock(Sender, Target: TObject; X,
  Y: Integer);
var
  i,w : integer;
begin
  w := 0;
  for i := 0 to ToolBarFont.ControlCount -1 do
    w := w + ToolBarFont.Controls[i].Width;

  ToolBarFont.Width := w ;
end;
procedure TTheMainForm.ToolBarViewEndDock(Sender, Target: TObject; X,
  Y: Integer);
var
  i,w : integer;
begin
  w := 0;
  for i := 0 to ToolBarView.ControlCount -1 do
    w := w + ToolBarView.Controls[i].Width;

  ToolBarView.Width := w ;
end;
procedure TTheMainForm.ToolBarEditEndDock(Sender, Target: TObject; X,
  Y: Integer);
var
  i,w : integer;
begin
  w := 0;
  for i := 0 to ToolBarEdit.ControlCount -1 do
    w := w + ToolBarEdit.Controls[i].Width;

  ToolBarEdit.Width := w ;
end;
procedure TTheMainForm.ToolBarFileEndDock(Sender, Target: TObject; X,
  Y: Integer);
var
  i,w : integer;
begin
  w := 0;
  for i := 0 to ToolBarFile.ControlCount -1 do
    w := w + ToolBarFile.Controls[i].Width;

  ToolBarFile.Width := w ;
end;
//------------------------------------------------------------------------------
//  Update the state of floating toolbars
//
procedure TTheMainForm.ToolbarButtonsClick(Sender: TObject);
var
  i, j  : integer;
  mItem : TMenuItem;
  Cntrl : TControl;
begin
  // Update any submeny

  for i := 0 to ToolBarButtons.Count - 1 do
    begin
      mItem := ToolBarButtons.Items[i];
      if mItem <> nil then
        begin
          if (CompareStr(mItem.Caption, 'File Band') = 0) then
            Cntrl := ToolBarFile
          else if (CompareStr(mItem.Caption, 'Edit Band') = 0) then
            Cntrl := ToolBarEdit
          else if (CompareStr(mItem.Caption, 'View Band') = 0) then
            Cntrl := ToolBarView
          else if (CompareStr(mItem.Caption, 'Edit Band') = 0) then
            Cntrl := ToolBarFile
          else if (CompareStr(mItem.Caption, 'Scale Band') = 0) then
            Cntrl := ToolBarScale
          else if (CompareStr(mItem.Caption, 'Font Band') = 0) then
            Cntrl := ToolBarFont
          else
            Cntrl := nil;
          if Cntrl = nil then break;

          // Test if its docked to the CoolBar

          if Cntrl.HostDockSite = CoolBar then
            begin
              // Find the band it is docked to

              for j := 0 to CoolBar.Bands.Count -1 do
                if CoolBar.Bands[j].Control = Cntrl then
                  begin
                    // Its docked to this band, Update the visibility

                    mItem.Checked :=  CoolBar.Bands[j].Visible;
                  end
            end
          else
            begin

              // Still here, Update the controls visibility instead

              mItem.Checked :=  Cntrl.Visible;
            end
        end;
    end;
end;
//------------------------------------------------------------------------------
// Save CoolBar settings to INI-File
//
procedure TTheMainForm.CoolBarIniSave (var Ini : TIniFile);
var
  i : integer;
  j : integer;
  w : TCustomDockForm;
  c : TControl;
begin

  Ini.EraseSection('CoolBar');

  // Walk all ToolBars and find out if they are in a CoolBand or floating

  for j := 0 to 4 do
    begin

      case j of
        0 : c := ToolBarFile;
        1 : c := ToolBarEdit;
        2 : c := ToolBarView;
        3 : c := ToolBarScale;
        4 : c := ToolBarFont;
      else
        c := nil;
      end;
      if c = nil then break;

      if c.HostDockSite is TCustomDockForm then
        begin
          w := TCustomDockForm(c.HostDockSite);
          Ini.WriteBool   ('CoolBar', c.Name + 'Float', true);
          Ini.WriteBool   ('CoolBar', c.Name + 'Visible', (fsVisible in w.FormState));
          Ini.WriteInteger('CoolBar', c.Name + 'Left', w.Left);
          Ini.WriteInteger('CoolBar', c.Name + 'Top', w.Top);
        end
      else
        begin

          // Find the CoolBand

          for i := 0 to CoolBar.Bands.Count -1 do
            if CoolBar.Bands[i].Control = c then
              begin
                Ini.WriteBool   ('CoolBar', c.Name + 'Float', false);
                Ini.WriteBool   ('CoolBar', c.Name + 'Visible', CoolBar.Bands[i].Visible);
                Ini.WriteInteger('CoolBar', c.Name + 'Band', i);
                Ini.WriteBool   ('CoolBar', c.Name + 'Break', CoolBar.Bands[i].Break);
                Ini.WriteInteger('CoolBar', c.Name + 'Width', CoolBar.Bands[i].Width);
                break;
              end;
        end;
    end;

  Ini.WriteBool( 'CoolBar', 'Total', ActionToolbarToggle.Checked);

end;
//------------------------------------------------------------------------------
// Load CoolBar settings from INI-File
//
procedure TTheMainForm.CoolBarIniLoad (var Ini : TIniFile);
var
  i,j : integer;
  c : TToolBar;
  wdt : integer;

  cArr : array [0..4] of TToolBar;
  vArr : array [0..4] of boolean;
  bArr : array [0..4] of boolean;
  wArr : array [0..4] of integer;

  rArr : array [0..4] of TRect;
  tArr : array [0..4] of TToolBar;
begin

  // Figure out if there is any ini settings

  if not Ini.SectionExists('CoolBar') then
    exit;

  // Initate all arrays (Have to do it this way, CoolBands are scary things
  // and resists heavily my attempts to be smart)

  for i := 0 to 4 do
    begin
      cArr[i] := nil;   // No toolbar
      tArr[i] := nil;
      vArr[i] := true;  // All visible
      bArr[i] := false; // No break
    end;

  // Walk all ToolBars and find out if they are in a CoolBand or floating

  for j := 0 to 4 do
    begin
      case j of
        0 : c := ToolBarFile;
        1 : c := ToolBarEdit;
        2 : c := ToolBarView;
        3 : c := ToolBarScale;
        4 : c := ToolBarFont;
      else
        c := nil;
      end;
      if c = nil then break;

      // Calculate the width of the ToolBar

      wdt := 0;
      for i := 0 to c.ControlCount - 1 do
        wdt := wdt + c.Controls[i].Width;

      // Is it floating or not

      if Ini.ReadBool('CoolBar', c.Name + 'Float', false) then
        begin
          // Remember this as a floating toolbar

          tArr[j] := c;

          rArr[j].Left   := Ini.ReadInteger('CoolBar', c.Name + 'Left', 100);
          rArr[j].Top    := Ini.ReadInteger('CoolBar', c.Name + 'Top', 100);
          rArr[j].Bottom := rArr[j].Top + c.Height;
          rArr[j].Right  := rArr[j].Left + wdt;

        end
      else
        begin

          // Place it in the right band

          i := Ini.ReadInteger('CoolBar', c.Name + 'Band', j);
          i := Max(0,i);
          i := Min(4,i);

          // Add it into the arrays at the right order

          cArr[i] := c;
          vArr[i] := Ini.ReadBool ('CoolBar', c.Name + 'Visible', true);
          bArr[i] := Ini.ReadBool ('CoolBar', c.Name + 'Break', false);
          wArr[i] := Ini.ReadInteger ('CoolBar', c.Name + 'Width', wdt);
        end;
    end;

  // Float some of the ttolbars

  for i := 0 to 4 do
    if tArr[i] <> nil then
      tArr[i].ManualFloat( rArr[i]);

  // Populate the bands in the right order

  for i := 0 to 4 do
    if cArr[i] <> nil then
      begin
        CoolBar.Bands[i].Control       := cArr[i];
        CoolBar.Bands[i].Visible       := vArr[i];
        CoolBar.Bands[i].Break         := bArr[i];

        wdt := 0;
          for j := 0 to CoolBar.Bands[i].Control.ControlCount - 1 do
            wdt := wdt + CoolBar.Bands[i].Control.Controls[j].Width;

        CoolBar.Bands[i].MinWidth      := wdt;
        CoolBar.Bands[i].Width         := wArr[i];
        CoolBar.Bands[i].Control.Width := wArr[i];
      end;

  // Load if CoolBar is visible at all

  ActionToolbarToggle.Checked := Ini.ReadBool( 'CoolBar', 'Total', true);
  CoolBar.Visible := ActionToolbarToggle.Checked;

end;
//------------------------------------------------------------------------------
// CoolBar has resized
//
procedure TTheMainForm.CoolBarResize(Sender: TObject);
begin
  GuiUpdateControls(1);
end;
//------------------------------------------------------------------------------
// Answer any ToolBar docking in a CoolBar
//
procedure TTheMainForm.CoolBarDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := (Source.Control is TToolBar);
  if Accept then
  begin

    // Modify the DockRect to preview dock area (Coolbar client area)

    ARect.TopLeft := CoolBar.ClientToScreen(CoolBar.ClientRect.TopLeft);
    ARect.BottomRight := CoolBar.ClientToScreen(CoolBar.ClientRect.BottomRight);
    Source.DockRect := ARect;
  end;
end;

//------------------------------------------------------------------------------
//                                   Undo
//------------------------------------------------------------------------------
// Action: Undo
//
procedure TTheMainForm.ActionUndoExecute(Sender: TObject);
var
  ud       : TMapItemUndoType;
  mp       : TMapItem;
  done     : boolean;
begin
  if MapItemList = nil then exit;

  done := false;

  // Undo any editing to the item

  mp := MapItemList.UndoLast(ud);
  if mp <> nil then
  begin
    case ud of
      udNewItem, udPasteItem:
        begin
          // Remove from listbox

          MapItemListBoxDel(mp);

          // Free the item

          mp.Free;

          done := true;
        end;
      udDelItem:
        begin

          // Add it back to the listbox

          MapItemListBoxAdd (mp);

          // Select it

          MapItemListBoxSel(mp);
          MapItemSelect (mp);

          // Redraw Map

          done := true;
        end;
      udMoveMid, udMoveItem, udRotateItem,
      udEditDesc, udScaleItem, udDelPnt, udMovePnt, udNewPnt:
        begin
          MapItemListBoxSel(mp);
          MapItemSelect (mp);
          done := true;
        end;
      udEditName:
        begin
          // Set new name on the listbox

          MapItemListBoxSetName (mp);

          MapItemListBoxSel(mp);
          MapItemSelect (mp);
          done := true;
        end;
    end;

    If done then
      begin
        // Redraw Map

        MapImageObj.BaseDraw;

        // Update Gui

        GuiUpdateItemSelection;
      end;
  end;
end;

procedure TTheMainForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
  mh, mw : integer;
begin

  if MapResizeFirst then
    MapResizeMapPos := MapImageObj.InqMapPntCenScr();
  MapResizeFirst := false;

  // Calculate Map Panel size

  mw := NewWidth - ListPanel.Width - MapSplitter.Width;
  mh := NewHeight - StatusBar.Height - CoolBar.Height -
            MapItemDesc.Height - DescSplitter.Height;

  if mw < 150 then
    NewWidth := ListPanel.Width + MapSplitter.Width + 150;

  if mh < 200 then
    NewHeight := StatusBar.Height + CoolBar.Height +
                  DescSplitter.Height + MapItemDesc.Height + 200;

  Resize := true;
end;

procedure TTheMainForm.ToolBarScaleClick(Sender: TObject);
begin

end;
//------------------------------------------------------------------------------
//                                 Preferenses
//------------------------------------------------------------------------------
// Action: Open Prderenses Diloag
//
procedure TTheMainForm.ActionPrefExecute(Sender: TObject);
var
  dlg : TPrefForm;
begin

  dlg := TPrefForm.Create(Self);
  dlg.Font := Self.Font;
  dlg.PrefInit(DrawNiftyRec);

  dlg.ShowModal;
  if dlg.PrefUpdate(DrawNiftyRec) then
    begin
      Self.Font := DrawNiftyRec.Font;
      FontSetCombo;
      GuiUpdateControls (0);
    end;

  dlg.Free;
end;
//------------------------------------------------------------------------------
// Initiate preferenses
//
procedure TTheMainForm.PrefInit;
begin
  with DrawNiftyRec do
    begin
      Increment     := 1;
      LineSpeed     := 20;
      LineCircleOn  := true;
      LineCircleWdt := 3;
      LineOn        := false;
      LineMapCenOn  := false;
      PointWdt      := 100;
      PointTurns    := 2;
      AreaRotateOn  := true;
      AreaRandOn    := true;
      AreaScaleInOn := true;
      AreaPolyOn    := true;
      Font          := Self.Font;
    end;
end;
//------------------------------------------------------------------------------
// Save Preferense settings to INI-File
//
procedure TTheMainForm.PrefIniSave (var Ini : TIniFile);
begin
  With DrawNiftyRec do
    begin
      Ini.WriteInteger ('Pref', 'LineSpeed',    LineSpeed);
      Ini.WriteBool    ('Pref', 'LineCircleOn', LineCircleOn);
      Ini.WriteInteger ('Pref', 'LineCircleWdt',LineCircleWdt);
      Ini.WriteBool    ('Pref', 'LineOn',       LineOn);
      Ini.WriteBool    ('Pref', 'LineMapCenOn', LineMapCenOn);
      Ini.WriteInteger ('Pref', 'PointWdt',     PointWdt);
      Ini.WriteInteger ('Pref', 'PointTurns',   PointTurns);
      Ini.WriteBool    ('Pref', 'AreaRotateOn', AreaRotateOn);
      Ini.WriteBool    ('Pref', 'AreaRandOn',   AreaRandOn);
      Ini.WriteBool    ('Pref', 'AreaScaleInOn',AreaScaleInOn);
      Ini.WriteBool    ('Pref', 'AreaPolyOn'   ,AreaPolyOn);
    end;
end;
//------------------------------------------------------------------------------
// Load Preferenses settings from INI-File
//
procedure TTheMainForm.PrefIniLoad (var Ini : TIniFile);
begin
  With DrawNiftyRec do
    begin
      LineSpeed     := Ini.ReadInteger ('Pref', 'LineSpeed',    LineSpeed);
      LineCircleOn  := Ini.ReadBool    ('Pref', 'LineCircleOn', LineCircleOn);
      LineCircleWdt := Ini.ReadInteger ('Pref', 'LineCircleWdt',LineCircleWdt);
      LineOn        := Ini.ReadBool    ('Pref', 'LineOn',       LineOn);
      LineMapCenOn  := Ini.ReadBool    ('Pref', 'LineMapCenOn', LineMapCenOn);
      PointWdt      := Ini.ReadInteger ('Pref', 'PointWdt',     PointWdt);
      PointTurns    := Ini.ReadInteger ('Pref', 'PointTurns',   PointTurns);
      AreaRotateOn  := Ini.ReadBool    ('Pref', 'AreaRotateOn', AreaRotateOn);
      AreaRandOn    := Ini.ReadBool    ('Pref', 'AreaRandOn',   AreaRandOn);
      AreaScaleInOn := Ini.ReadBool    ('Pref', 'AreaScaleInOn',AreaScaleInOn);
      AreaPolyOn    := Ini.ReadBool    ('Pref', 'AreaPolyOn',   AreaPolyOn);
    end;
end;


initialization

// Add all necessary leafs for this object

  LeafAdd (LeafsMap, LeafMapInfo,      'MapInfo',      atUnknown);
  LeafAdd (LeafsMap, LeafMapName,      'MapName',      atString);
  LeafAdd (LeafsMap, LeafItemTypeList, 'ItemTypeList', atUnknown);
  LeafAdd (LeafsMap, LeafItemList,     'ItemList',     atUnknown);


end.
//------------------------------------------------------------------------------

