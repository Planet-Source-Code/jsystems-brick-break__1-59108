VERSION 5.00
Begin VB.Form frmPreEdit 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Editor settings"
   ClientHeight    =   2370
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4860
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2370
   ScaleWidth      =   4860
   StartUpPosition =   2  'CenterScreen
   Begin VB.DirListBox Dir1 
      Height          =   540
      Left            =   120
      TabIndex        =   7
      Top             =   1680
      Visible         =   0   'False
      Width           =   795
   End
   Begin VB.CommandButton Command3 
      Caption         =   "EXIT"
      Height          =   315
      Left            =   2520
      TabIndex        =   6
      Top             =   1980
      Width           =   1155
   End
   Begin VB.CommandButton Command2 
      Caption         =   "START"
      Height          =   315
      Left            =   3720
      TabIndex        =   5
      Top             =   1980
      Width           =   1095
   End
   Begin VB.ComboBox Combo2 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   315
      Left            =   1200
      TabIndex        =   4
      Text            =   "Combo2"
      Top             =   840
      Width           =   1635
   End
   Begin VB.CommandButton Command1 
      Caption         =   "NEW GAME"
      Height          =   315
      Left            =   3000
      TabIndex        =   2
      Top             =   240
      Width           =   1395
   End
   Begin VB.ComboBox Combo1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   315
      Left            =   1200
      TabIndex        =   0
      Text            =   "Combo1"
      Top             =   240
      Width           =   1635
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Level:"
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
      TabIndex        =   3
      Top             =   840
      Width           =   720
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
      Left            =   240
      TabIndex        =   1
      Top             =   240
      Width           =   810
   End
End
Attribute VB_Name = "frmPreEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Dim xName As String
xName = InputBox("Game name", "Create new game")
If xName = "" Then Exit Sub
Dim Fs As String
Fs = App.Path & "\level\" & xName
If Dir(Fs, vbDirectory) = "" Then
MkDir Fs
Dir1.Path = Fs
Dir1.Refresh
Combo1.Clear
For i = 0 To Dir1.ListCount - 1
Combo1.AddItem Termin(Dir1.List(i))
Next
Else
MsgBox "Game " & xName & " already exists.", vbCritical
End If
End Sub

Private Sub Command2_Click()
GameName = Combo1.Text
LevelX = Val(Combo2.Text)
Unload Me
frmEdit.Show
End Sub

Private Sub Command3_Click()
Unload Me
frmStart.Show
End Sub

Private Sub Form_Load()
Dim Fs As String
Fs = App.Path & "\level\"
Dir1.Path = Fs
For i = 0 To Dir1.ListCount - 1
Combo1.AddItem Termin(Dir1.List(i))
Next
For i = 1 To 50
Combo2.AddItem i
Next
Combo1.Text = Combo1.List(0)
Combo2.Text = Combo2.List(0)
End Sub

Private Function Termin(ByVal P As String) As String
Dim i As Integer
Dim n As Integer
For i = Len(P) To 1 Step -1
    n = n + 1
    If Mid(P, i, 1) = "\" Then
    Exit For
    End If
Next
Termin = Right(P, n - 1)
End Function
