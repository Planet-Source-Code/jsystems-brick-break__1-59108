VERSION 5.00
Begin VB.Form frmStart 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   4515
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   3285
   LinkTopic       =   "Form1"
   ScaleHeight     =   4515
   ScaleWidth      =   3285
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.DirListBox Dir1 
      Height          =   540
      Left            =   3840
      TabIndex        =   6
      Top             =   4740
      Width           =   975
   End
   Begin VB.ComboBox Combo1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   315
      Left            =   1260
      TabIndex        =   5
      Text            =   "Combo1"
      Top             =   360
      Width           =   1635
   End
   Begin VB.Label Label6 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "QUICK HELP"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   360
      Left            =   720
      TabIndex        =   7
      Top             =   3060
      Width           =   1830
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "EXIT"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   360
      Left            =   1260
      TabIndex        =   4
      Top             =   3900
      Width           =   720
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "HIGH SCORES"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   360
      Left            =   600
      TabIndex        =   3
      Top             =   2280
      Width           =   2115
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "LEVEL EDITOR"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   360
      Left            =   540
      TabIndex        =   2
      Top             =   1620
      Width           =   2205
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "START GAME"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   360
      Left            =   660
      TabIndex        =   1
      Top             =   1020
      Width           =   2010
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Game:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   300
      Left            =   300
      TabIndex        =   0
      Top             =   360
      Width           =   810
   End
End
Attribute VB_Name = "frmStart"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
Label2.ForeColor = vbGreen
Label3.ForeColor = vbGreen
Label4.ForeColor = vbGreen
Label5.ForeColor = vbGreen
Label6.ForeColor = vbGreen

Dim Fs As String
Fs = App.Path & "\level\"
Dir1.Path = Fs
For I = 0 To Dir1.ListCount - 1
Combo1.AddItem Termin(Dir1.List(I))
Next
Combo1.Text = Combo1.List(0)
If FileExists(App.Path & "\brix.ini") Then
Else
Call WriteINI("Scores", "Score1", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score2", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score3", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score4", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score5", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", "0", App.Path & "\brix.ini")
Call WriteINI("Scores", "Score10", "0", App.Path & "\brix.ini")
Call WriteINI("Names", "Score1", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score2", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score3", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score4", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score5", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", "Empty", App.Path & "\brix.ini")
Call WriteINI("Names", "Score10", "Empty", App.Path & "\brix.ini")
End If

End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Label2.ForeColor = vbGreen
Label3.ForeColor = vbGreen
Label4.ForeColor = vbGreen
Label5.ForeColor = vbGreen
Label6.ForeColor = vbGreen

End Sub

Private Sub Label2_Click()
GameName = Combo1.Text
If GameName = "" Then GameName = "demo"
LevelX = 1
Unload Me
frmGame.Show
End Sub

Private Sub Label2_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Label2.ForeColor = vbRed
End Sub

Private Sub Label3_Click()
Unload Me
frmPreEdit.Show
End Sub

Private Sub Label3_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Label3.ForeColor = vbRed
End Sub

Private Sub Label4_Click()
frmScores.Show
End Sub

Private Sub Label4_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Label4.ForeColor = vbRed
End Sub

Private Sub Label5_Click()
Unload Me
End
End Sub

Private Sub Label5_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Label5.ForeColor = vbRed
End Sub

Private Function Termin(ByVal P As String) As String
Dim I As Integer
Dim n As Integer
For I = Len(P) To 1 Step -1
    n = n + 1
    If Mid(P, I, 1) = "\" Then
    Exit For
    End If
Next
Termin = Right(P, n - 1)
End Function

Public Function FileExists(FileNa As String) As Boolean
    Dim FRes As String
    On Error GoTo NotFound
    FRes = Dir$(FileNa)
    If FRes = "" Then FileExists = False Else FileExists = True
NotFound:
    If Err = 53 Then Resume Next
End Function
Public Function ReadINI(strsection As String, strkey As String, strfullpath As String) As String
   Dim strbuffer As String
   Let strbuffer$ = String$(750, Chr$(0&))
   Let ReadINI$ = Left$(strbuffer$, GetPrivateProfileString(strsection$, ByVal LCase$(strkey$), "", strbuffer, Len(strbuffer), strfullpath$))
End Function
Public Sub WriteINI(strsection As String, strkey As String, strkeyvalue As String, strfullpath As String)
    Call WritePrivateProfileString(strsection$, UCase$(strkey$), strkeyvalue$, strfullpath$)
End Sub

Private Sub Label6_Click()
frmHelp.Show
End Sub

Private Sub Label6_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Label6.ForeColor = vbRed
End Sub
