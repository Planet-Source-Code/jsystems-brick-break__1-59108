VERSION 5.00
Begin VB.Form frmScores 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "HIGH SCORES"
   ClientHeight    =   4890
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4650
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4890
   ScaleWidth      =   4650
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "EXIT"
      Height          =   255
      Left            =   1620
      TabIndex        =   10
      Top             =   4560
      Width           =   1455
   End
   Begin VB.Label scores2 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2340
      TabIndex        =   30
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores3 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2940
      TabIndex        =   29
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores4 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   3540
      TabIndex        =   28
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores5 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4140
      TabIndex        =   27
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores6 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4740
      TabIndex        =   26
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores7 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5340
      TabIndex        =   25
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores8 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5940
      TabIndex        =   24
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores9 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   6540
      TabIndex        =   23
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores10 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7140
      TabIndex        =   22
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label scores1 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   1740
      TabIndex        =   21
      Top             =   6780
      Width           =   495
   End
   Begin VB.Label names2 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2340
      TabIndex        =   20
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names3 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2940
      TabIndex        =   19
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names4 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   3540
      TabIndex        =   18
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names5 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4140
      TabIndex        =   17
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names7 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5340
      TabIndex        =   16
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names8 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5940
      TabIndex        =   15
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names9 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   6540
      TabIndex        =   14
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names10 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7140
      TabIndex        =   13
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names6 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4740
      TabIndex        =   12
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label names1 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   1740
      TabIndex        =   11
      Top             =   7140
      Width           =   495
   End
   Begin VB.Label level10 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label10"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   9
      Top             =   3780
      Width           =   840
   End
   Begin VB.Label level9 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label9"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   8
      Top             =   3360
      Width           =   720
   End
   Begin VB.Label level8 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label8"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   7
      Top             =   2940
      Width           =   720
   End
   Begin VB.Label level7 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label7"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   6
      Top             =   2520
      Width           =   720
   End
   Begin VB.Label level6 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   5
      Top             =   2100
      Width           =   720
   End
   Begin VB.Label level5 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label5"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   4
      Top             =   1680
      Width           =   720
   End
   Begin VB.Label level4 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label4"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   3
      Top             =   1260
      Width           =   720
   End
   Begin VB.Label level3 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label3"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   2
      Top             =   900
      Width           =   720
   End
   Begin VB.Label level2 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label2"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   1
      Top             =   540
      Width           =   720
   End
   Begin VB.Label level1 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Label1"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   240
      Left            =   180
      TabIndex        =   0
      Top             =   180
      Width           =   720
   End
End
Attribute VB_Name = "frmScores"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Unload Me
frmStart.Show

End Sub

Private Sub Form_Load()
FormOnTop Me
scores10.Caption = ReadINI("Scores", "Score10", App.Path & "\brix.ini")
scores9.Caption = ReadINI("Scores", "Score9", App.Path & "\brix.ini")
scores8.Caption = ReadINI("Scores", "Score8", App.Path & "\brix.ini")
scores7.Caption = ReadINI("Scores", "Score7", App.Path & "\brix.ini")
scores6.Caption = ReadINI("Scores", "Score6", App.Path & "\brix.ini")
scores5.Caption = ReadINI("Scores", "Score5", App.Path & "\brix.ini")
scores4.Caption = ReadINI("Scores", "Score4", App.Path & "\brix.ini")
scores3.Caption = ReadINI("Scores", "Score3", App.Path & "\brix.ini")
scores2.Caption = ReadINI("Scores", "Score2", App.Path & "\brix.ini")
scores1.Caption = ReadINI("Scores", "Score1", App.Path & "\brix.ini")

names10.Caption = ReadINI("Names", "Score10", App.Path & "\brix.ini")
names9.Caption = ReadINI("Names", "Score9", App.Path & "\brix.ini")
names8.Caption = ReadINI("Names", "Score8", App.Path & "\brix.ini")
names7.Caption = ReadINI("Names", "Score7", App.Path & "\brix.ini")
names6.Caption = ReadINI("Names", "Score6", App.Path & "\brix.ini")
names5.Caption = ReadINI("Names", "Score5", App.Path & "\brix.ini")
names4.Caption = ReadINI("Names", "Score4", App.Path & "\brix.ini")
names3.Caption = ReadINI("Names", "Score3", App.Path & "\brix.ini")
names2.Caption = ReadINI("Names", "Score2", App.Path & "\brix.ini")
names1.Caption = ReadINI("Names", "Score1", App.Path & "\brix.ini")

level1.Caption = " 1. " & names1.Caption & " - " & scores1.Caption
level2.Caption = " 2. " & names2.Caption & " - " & scores2.Caption
level3.Caption = " 3. " & names3.Caption & " - " & scores3.Caption
level4.Caption = " 4. " & names4.Caption & " - " & scores4.Caption
level5.Caption = " 5. " & names5.Caption & " - " & scores5.Caption
level6.Caption = " 6. " & names6.Caption & " - " & scores6.Caption
level7.Caption = " 7. " & names7.Caption & " - " & scores7.Caption
level8.Caption = " 8. " & names8.Caption & " - " & scores8.Caption
level9.Caption = " 9. " & names9.Caption & " - " & scores9.Caption
level10.Caption = " 10. " & names10.Caption & " - " & scores10.Caption
End Sub

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
