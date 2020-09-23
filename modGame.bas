Attribute VB_Name = "modGame"
'//Clip Cursor API Declaration
Public Declare Function ClipCursor Lib "user32" (lpRect As _
Any) As Long
Public Declare Function SetWindowPos Lib "user32" _
(ByVal hWnd As Long, ByVal hWndInsertAfter As Long, ByVal _
X As Long, ByVal Y As Long, ByVal cx As Long, ByVal cy As _
Long, ByVal wFlags As Long) As Long
Public Const HWND_TOPMOST = -1
Public Const HWND_NOTOPMOST = -2
Public Const SWP_NOSIZE = &H1
Public Const SWP_NOMOVE = &H2
Public Const FLAGS = SWP_NOSIZE Or SWP_NOMOVE
Public Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Public Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpString As Any, ByVal lpFileName As String) As Long

'//Clip Cursor Variables
Public lTwipsX As Long
Public lTwipsY As Long

Public RectArea As RECT

'//Clip Cursor Type
Public Type RECT
    Left   As Long
    Top    As Long
    Right  As Long
    Bottom As Long
End Type
Public Declare Function GetCursorPos Lib "user32" (lpPoint _
As POINTAPI) As Long
Public Declare Function SetCursorPos Lib "user32" _
(ByVal X As Long, ByVal Y As Long) As Long
Public MousePos As POINTAPI
Public XPos As Integer
Public YPos As Integer

'//Get Cursor Position Type
Public Type POINTAPI
  X As Long
  Y As Long
End Type


'//Trap Cursor Function
Public Function TrapCursor(frm As Form)
    '//Assigns values to variables
    lTwipsX = Screen.TwipsPerPixelX
    lTwipsY = Screen.TwipsPerPixelY

'//Assigns values to Type(This is the forms measurments)
With RectArea
    .Left = frm.Left / lTwipsX
    .Top = frm.Top / lTwipsY
    .Right = .Left + frm.Width / lTwipsX
    .Bottom = .Top + frm.Height / lTwipsY
End With

'//Calls API
Call ClipCursor(RectArea)
End Function

Public Function ReleaseCursor(frm As Form)
    '//Assigns values to variables
    lTwipsX = Screen.TwipsPerPixelX
    lTwipsY = Screen.TwipsPerPixelY

'//Assigns values to Type
With RectArea
    .Left = 0
    .Top = 0
    .Right = Screen.Width / lTwipsX
    .Bottom = Screen.Height / lTwipsY
End With

'//Calls API
Call ClipCursor(RectArea)
End Function


Public Sub GetCursorPosition()
    GetCursorPos MousePos ' Get Co-ordinates
    XPos = MousePos.X ' Get X co-ordinates
    YPos = MousePos.Y ' Get Y co-ordinates
End Sub

Public Function MoveCursor(X As Integer, Y As Integer)
Dim Pos As Integer
    
    Pos = SetCursorPos(X, Y) 'Calls API
    
End Function

Public Function FormOnTop(frm As Form)
Dim SetFrmOnTop As Long
    SetFrmOnTop = SetWindowPos(frm.hWnd, HWND_TOPMOST, 0, 0, _
                    0, 0, FLAGS)
End Function

Public Function FormNotOnTop(frm As Form)
Dim SetFrmNotOnTop As Long
    SetFrmNotOnTop = SetWindowPos(frm.hWnd, HWND_NOTOPMOST, 0, _
                        0, 0, 0, FLAGS)
End Function

