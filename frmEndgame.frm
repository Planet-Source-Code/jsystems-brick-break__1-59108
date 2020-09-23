VERSION 5.00
Begin VB.Form frmEndgame 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "End game"
   ClientHeight    =   3075
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4845
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3075
   ScaleWidth      =   4845
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   375
      Left            =   1800
      TabIndex        =   4
      Top             =   2640
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   11.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   405
      Left            =   1800
      MaxLength       =   18
      TabIndex        =   0
      Top             =   1740
      Width           =   2415
   End
   Begin VB.Label tempscores 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7620
      TabIndex        =   15
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores2 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   1020
      TabIndex        =   14
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores3 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   1620
      TabIndex        =   13
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores4 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2220
      TabIndex        =   12
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores5 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2820
      TabIndex        =   11
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores6 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   3420
      TabIndex        =   10
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores7 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4020
      TabIndex        =   9
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores8 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4620
      TabIndex        =   8
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores9 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5220
      TabIndex        =   7
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores10 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5820
      TabIndex        =   6
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label scores1 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   420
      TabIndex        =   5
      Top             =   4560
      Width           =   495
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      Caption         =   "Game Over!"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   495
      Left            =   0
      TabIndex        =   3
      Top             =   180
      Width           =   4815
   End
   Begin VB.Label Label7 
      Alignment       =   2  'Center
      BackColor       =   &H00E0E0E0&
      Caption         =   "Score: x"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   18
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   495
      Left            =   720
      TabIndex        =   2
      Top             =   840
      Visible         =   0   'False
      Width           =   3255
   End
   Begin VB.Label Label5 
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "Name:"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   14.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   375
      Left            =   480
      TabIndex        =   1
      Top             =   1740
      Width           =   1095
   End
End
Attribute VB_Name = "frmEndgame"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
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

Private Sub Command1_Click()
Dohighscores
End Sub

Public Sub Form_Load()
FormOnTop Me

If frmGame.Tag <> "" Then
Label1.Caption = "New High Score!"
End If
Label7.Caption = "Score: " & ScoreX
End Sub


Private Sub Dohighscores()
Dim score1, score2, score3, score4, score5, score6, score7, score8, score9, score10, tempscore As Integer
score10 = ReadINI("Names", "Score10", App.Path & "\brix.ini")
score9 = ReadINI("Names", "Score9", App.Path & "\brix.ini")
score8 = ReadINI("Names", "Score8", App.Path & "\brix.ini")
score7 = ReadINI("Names", "Score7", App.Path & "\brix.ini")
score6 = ReadINI("Names", "Score6", App.Path & "\brix.ini")
score5 = ReadINI("Names", "Score5", App.Path & "\brix.ini")
score4 = ReadINI("Names", "Score4", App.Path & "\brix.ini")
score3 = ReadINI("Names", "Score3", App.Path & "\brix.ini")
score2 = ReadINI("Names", "Score2", App.Path & "\brix.ini")
score1 = ReadINI("Names", "Score1", App.Path & "\brix.ini")
scores1.Caption = score1
scores2.Caption = score2
scores3.Caption = score3
scores4.Caption = score4
scores5.Caption = score5
scores6.Caption = score6
scores7.Caption = score7
scores8.Caption = score8
scores9.Caption = score9
scores10.Caption = score10

If Text1.Text = "" Then
MsgBox "Please enter you're name!", vbCritical
Exit Sub
Else

If frmGame.Tag = "score1" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score4", scores3.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score3", scores2.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score2", scores1.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score1", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score2" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score4", scores3.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score3", scores2.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score2", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score3" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score4", scores3.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score3", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score4" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score4", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score5" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score5", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score6" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score6", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score7" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score7", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score8" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score8", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score9" Then
Call WriteINI("Names", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Names", "Score9", Text1.Text, App.Path & "\brix.ini")
End If

If frmGame.Tag = "score10" Then
Call WriteINI("Names", "Score10", Text1.Text, App.Path & "\brix.ini")
End If
End If
Unload Me
frmScores.Show
End Sub


