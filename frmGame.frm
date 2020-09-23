VERSION 5.00
Begin VB.Form frmGame 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Brickbreak 3 by JSystems"
   ClientHeight    =   8040
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   10230
   Icon            =   "frmGame.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8040
   ScaleWidth      =   10230
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Fal 
      AutoSize        =   -1  'True
      Height          =   360
      Left            =   -60
      Picture         =   "frmGame.frx":1CFA
      ScaleHeight     =   300
      ScaleWidth      =   15195
      TabIndex        =   20
      Top             =   7740
      Visible         =   0   'False
      Width           =   15255
   End
   Begin VB.Timer TT3 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   3420
      Top             =   7620
   End
   Begin VB.Timer TT2 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   2880
      Top             =   7620
   End
   Begin VB.Timer TT1 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   2340
      Top             =   7560
   End
   Begin VB.Timer Timer3 
      Interval        =   1
      Left            =   8640
      Top             =   7500
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   120
      Top             =   7560
   End
   Begin VB.Timer Timer2 
      Interval        =   1
      Left            =   600
      Top             =   7560
   End
   Begin VB.PictureBox ball 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   240
      Left            =   5220
      MouseIcon       =   "frmGame.frx":5710
      MousePointer    =   99  'Custom
      Picture         =   "frmGame.frx":5862
      ScaleHeight     =   240
      ScaleMode       =   0  'User
      ScaleWidth      =   9440
      TabIndex        =   1
      Top             =   7200
      Width           =   240
   End
   Begin VB.PictureBox bouncer 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   4260
      MouseIcon       =   "frmGame.frx":5C93
      MousePointer    =   99  'Custom
      Picture         =   "frmGame.frx":5DE5
      ScaleHeight     =   225
      ScaleMode       =   0  'User
      ScaleWidth      =   1456.311
      TabIndex        =   0
      Top             =   7500
      Width           =   2250
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   139
      Left            =   8160
      Picture         =   "frmGame.frx":6294
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   138
      Left            =   9180
      Picture         =   "frmGame.frx":66FD
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   137
      Left            =   6120
      Picture         =   "frmGame.frx":6B66
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   136
      Left            =   2040
      Picture         =   "frmGame.frx":6FCF
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   135
      Left            =   5100
      Picture         =   "frmGame.frx":7438
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   134
      Left            =   1020
      Picture         =   "frmGame.frx":78A1
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   133
      Left            =   1020
      Picture         =   "frmGame.frx":7D0A
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   132
      Left            =   2040
      Picture         =   "frmGame.frx":8173
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   131
      Left            =   3060
      Picture         =   "frmGame.frx":85DC
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   130
      Left            =   4080
      Picture         =   "frmGame.frx":8A45
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   129
      Left            =   5100
      Picture         =   "frmGame.frx":8EAE
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   128
      Left            =   6120
      Picture         =   "frmGame.frx":9317
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   127
      Left            =   7140
      Picture         =   "frmGame.frx":9780
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   126
      Left            =   8160
      Picture         =   "frmGame.frx":9BE9
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   125
      Left            =   9180
      Picture         =   "frmGame.frx":A052
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   124
      Left            =   7140
      Picture         =   "frmGame.frx":A4BB
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   123
      Left            =   4080
      Picture         =   "frmGame.frx":A924
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   122
      Left            =   3060
      Picture         =   "frmGame.frx":AD8D
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   121
      Left            =   2040
      Picture         =   "frmGame.frx":B1F6
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   120
      Left            =   5100
      Picture         =   "frmGame.frx":B65F
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   119
      Left            =   5100
      Picture         =   "frmGame.frx":BAC8
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   118
      Left            =   3060
      Picture         =   "frmGame.frx":BF31
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   117
      Left            =   1020
      Picture         =   "frmGame.frx":C39A
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   116
      Left            =   0
      Picture         =   "frmGame.frx":C803
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   115
      Left            =   0
      Picture         =   "frmGame.frx":CC6C
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   114
      Left            =   4080
      Picture         =   "frmGame.frx":D0D5
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   113
      Left            =   0
      Picture         =   "frmGame.frx":D53E
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   112
      Left            =   7140
      Picture         =   "frmGame.frx":D9A7
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   111
      Left            =   9180
      Picture         =   "frmGame.frx":DE10
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   110
      Left            =   6120
      Picture         =   "frmGame.frx":E279
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   109
      Left            =   8160
      Picture         =   "frmGame.frx":E6E2
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   108
      Left            =   1020
      Picture         =   "frmGame.frx":EB4B
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   107
      Left            =   3060
      Picture         =   "frmGame.frx":EFB4
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   106
      Left            =   0
      Picture         =   "frmGame.frx":F41D
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   105
      Left            =   4080
      Picture         =   "frmGame.frx":F886
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   104
      Left            =   2040
      Picture         =   "frmGame.frx":FCEF
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   103
      Left            =   6120
      Picture         =   "frmGame.frx":10158
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   102
      Left            =   7140
      Picture         =   "frmGame.frx":105C1
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   101
      Left            =   9180
      Picture         =   "frmGame.frx":10A2A
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   100
      Left            =   8160
      Picture         =   "frmGame.frx":10E93
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   38
      Left            =   5760
      Picture         =   "frmGame.frx":112FC
      Tag             =   "1"
      Top             =   5280
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   37
      Left            =   660
      Picture         =   "frmGame.frx":11606
      Tag             =   "9"
      Top             =   4980
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   36
      Left            =   1680
      Picture         =   "frmGame.frx":11910
      Tag             =   "9"
      Top             =   4080
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   35
      Left            =   8820
      Picture         =   "frmGame.frx":11C1A
      Tag             =   "6"
      Top             =   5280
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   34
      Left            =   3720
      Picture         =   "frmGame.frx":11F24
      Tag             =   "2"
      Top             =   5280
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   33
      Left            =   4740
      Picture         =   "frmGame.frx":1222E
      Tag             =   "4"
      Top             =   4680
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   32
      Left            =   7800
      Picture         =   "frmGame.frx":12538
      Tag             =   "5"
      Top             =   5880
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   31
      Left            =   6780
      Picture         =   "frmGame.frx":12842
      Tag             =   "7"
      Top             =   4380
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   30
      Left            =   8820
      Picture         =   "frmGame.frx":12B4C
      Tag             =   "3"
      Top             =   6480
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   29
      Left            =   1680
      Picture         =   "frmGame.frx":12E56
      Tag             =   "7"
      Top             =   5280
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   28
      Left            =   5760
      Picture         =   "frmGame.frx":13160
      Tag             =   "8"
      Top             =   6180
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   27
      Left            =   1680
      Picture         =   "frmGame.frx":1346A
      Tag             =   "1"
      Top             =   6480
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   26
      Left            =   4740
      Picture         =   "frmGame.frx":13774
      Tag             =   "6"
      Top             =   6480
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   25
      Left            =   5280
      Picture         =   "frmGame.frx":13A7E
      Tag             =   "1"
      Top             =   4860
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   24
      Left            =   180
      Picture         =   "frmGame.frx":13D88
      Tag             =   "9"
      Top             =   4560
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   23
      Left            =   1200
      Picture         =   "frmGame.frx":14092
      Tag             =   "9"
      Top             =   3660
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   22
      Left            =   8340
      Picture         =   "frmGame.frx":1439C
      Tag             =   "6"
      Top             =   4860
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   21
      Left            =   3240
      Picture         =   "frmGame.frx":146A6
      Tag             =   "2"
      Top             =   4860
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   20
      Left            =   4260
      Picture         =   "frmGame.frx":149B0
      Tag             =   "4"
      Top             =   4260
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   19
      Left            =   7320
      Picture         =   "frmGame.frx":14CBA
      Tag             =   "5"
      Top             =   5460
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   18
      Left            =   6300
      Picture         =   "frmGame.frx":14FC4
      Tag             =   "7"
      Top             =   3960
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   17
      Left            =   8340
      Picture         =   "frmGame.frx":152CE
      Tag             =   "3"
      Top             =   6060
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   16
      Left            =   1200
      Picture         =   "frmGame.frx":155D8
      Tag             =   "7"
      Top             =   4860
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   15
      Left            =   5280
      Picture         =   "frmGame.frx":158E2
      Tag             =   "8"
      Top             =   5760
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   14
      Left            =   1200
      Picture         =   "frmGame.frx":15BEC
      Tag             =   "1"
      Top             =   6060
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   13
      Left            =   4260
      Picture         =   "frmGame.frx":15EF6
      Tag             =   "6"
      Top             =   6060
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   12
      Left            =   5640
      Picture         =   "frmGame.frx":16200
      Tag             =   "6"
      Top             =   6600
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   11
      Left            =   2580
      Picture         =   "frmGame.frx":1650A
      Tag             =   "1"
      Top             =   6600
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   10
      Left            =   6660
      Picture         =   "frmGame.frx":16814
      Tag             =   "8"
      Top             =   6300
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   9
      Left            =   2580
      Picture         =   "frmGame.frx":16B1E
      Tag             =   "7"
      Top             =   5400
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   8
      Left            =   9720
      Picture         =   "frmGame.frx":16E28
      Tag             =   "3"
      Top             =   6600
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   7
      Left            =   7680
      Picture         =   "frmGame.frx":17132
      Tag             =   "7"
      Top             =   4500
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   6
      Left            =   8700
      Picture         =   "frmGame.frx":1743C
      Tag             =   "5"
      Top             =   6000
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   5
      Left            =   5640
      Picture         =   "frmGame.frx":17746
      Tag             =   "4"
      Top             =   4800
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   4
      Left            =   4620
      Picture         =   "frmGame.frx":17A50
      Tag             =   "2"
      Top             =   5400
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   3
      Left            =   9720
      Picture         =   "frmGame.frx":17D5A
      Tag             =   "6"
      Top             =   5400
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   2
      Left            =   2580
      Picture         =   "frmGame.frx":18064
      Tag             =   "9"
      Top             =   4200
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   1
      Left            =   1560
      Picture         =   "frmGame.frx":1836E
      Tag             =   "9"
      Top             =   5100
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   0
      Left            =   6660
      Picture         =   "frmGame.frx":18678
      Tag             =   "1"
      Top             =   5400
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label scores1 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   720
      TabIndex        =   19
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores10 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   6120
      TabIndex        =   18
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores9 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5520
      TabIndex        =   17
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores8 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4920
      TabIndex        =   16
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores7 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4320
      TabIndex        =   15
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores6 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   3720
      TabIndex        =   14
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores5 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   3120
      TabIndex        =   13
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores4 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2520
      TabIndex        =   12
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores3 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   1920
      TabIndex        =   11
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label scores2 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   1320
      TabIndex        =   10
      Top             =   8160
      Width           =   495
   End
   Begin VB.Label tempscores 
      BackColor       =   &H00000000&
      Caption         =   "Label6"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7920
      TabIndex        =   9
      Top             =   8160
      Width           =   495
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   99
      Left            =   0
      Picture         =   "frmGame.frx":18982
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   98
      Left            =   1020
      Picture         =   "frmGame.frx":18DEB
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   97
      Left            =   2040
      Picture         =   "frmGame.frx":19254
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   96
      Left            =   8160
      Picture         =   "frmGame.frx":196BD
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   95
      Left            =   5100
      Picture         =   "frmGame.frx":19B26
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   94
      Left            =   9180
      Picture         =   "frmGame.frx":19F8F
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   93
      Left            =   7140
      Picture         =   "frmGame.frx":1A3F8
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   92
      Left            =   7140
      Picture         =   "frmGame.frx":1A861
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   91
      Left            =   6120
      Picture         =   "frmGame.frx":1ACCA
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   90
      Left            =   3060
      Picture         =   "frmGame.frx":1B133
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   89
      Left            =   4080
      Picture         =   "frmGame.frx":1B59C
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   88
      Left            =   7140
      Picture         =   "frmGame.frx":1BA05
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   87
      Left            =   2040
      Picture         =   "frmGame.frx":1BE6E
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   86
      Left            =   4080
      Picture         =   "frmGame.frx":1C2D7
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   85
      Left            =   6120
      Picture         =   "frmGame.frx":1C740
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   84
      Left            =   2040
      Picture         =   "frmGame.frx":1CBA9
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   83
      Left            =   0
      Picture         =   "frmGame.frx":1D012
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   82
      Left            =   3060
      Picture         =   "frmGame.frx":1D47B
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   81
      Left            =   1020
      Picture         =   "frmGame.frx":1D8E4
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   80
      Left            =   9180
      Picture         =   "frmGame.frx":1DD4D
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   79
      Left            =   8160
      Picture         =   "frmGame.frx":1E1B6
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   78
      Left            =   6120
      Picture         =   "frmGame.frx":1E61F
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   77
      Left            =   4080
      Picture         =   "frmGame.frx":1EA88
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   76
      Left            =   9180
      Picture         =   "frmGame.frx":1EEF1
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   75
      Left            =   7140
      Picture         =   "frmGame.frx":1F35A
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   74
      Left            =   0
      Picture         =   "frmGame.frx":1F7C3
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   73
      Left            =   4080
      Picture         =   "frmGame.frx":1FC2C
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   72
      Left            =   0
      Picture         =   "frmGame.frx":20095
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   71
      Left            =   0
      Picture         =   "frmGame.frx":204FE
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   70
      Left            =   8160
      Picture         =   "frmGame.frx":20967
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   69
      Left            =   1020
      Picture         =   "frmGame.frx":20DD0
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   68
      Left            =   0
      Picture         =   "frmGame.frx":21239
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   67
      Left            =   3060
      Picture         =   "frmGame.frx":216A2
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   66
      Left            =   5100
      Picture         =   "frmGame.frx":21B0B
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   65
      Left            =   5100
      Picture         =   "frmGame.frx":21F74
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   64
      Left            =   2040
      Picture         =   "frmGame.frx":223DD
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   63
      Left            =   3060
      Picture         =   "frmGame.frx":22846
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   62
      Left            =   4080
      Picture         =   "frmGame.frx":22CAF
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   61
      Left            =   9180
      Picture         =   "frmGame.frx":23118
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   60
      Left            =   1020
      Picture         =   "frmGame.frx":23581
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   59
      Left            =   7140
      Picture         =   "frmGame.frx":239EA
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   58
      Left            =   5100
      Picture         =   "frmGame.frx":23E53
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   57
      Left            =   9180
      Picture         =   "frmGame.frx":242BC
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   56
      Left            =   8160
      Picture         =   "frmGame.frx":24725
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   55
      Left            =   7140
      Picture         =   "frmGame.frx":24B8E
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   54
      Left            =   6120
      Picture         =   "frmGame.frx":24FF7
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   53
      Left            =   5100
      Picture         =   "frmGame.frx":25460
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   52
      Left            =   4080
      Picture         =   "frmGame.frx":258C9
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   51
      Left            =   3060
      Picture         =   "frmGame.frx":25D32
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   50
      Left            =   2040
      Picture         =   "frmGame.frx":2619B
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   49
      Left            =   8160
      Picture         =   "frmGame.frx":26604
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   48
      Left            =   3060
      Picture         =   "frmGame.frx":26A6D
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   47
      Left            =   3060
      Picture         =   "frmGame.frx":26ED6
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   46
      Left            =   5100
      Picture         =   "frmGame.frx":2733F
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   45
      Left            =   6120
      Picture         =   "frmGame.frx":277A8
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   44
      Left            =   7140
      Picture         =   "frmGame.frx":27C11
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   43
      Left            =   8160
      Picture         =   "frmGame.frx":2807A
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   42
      Left            =   1020
      Picture         =   "frmGame.frx":284E3
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   41
      Left            =   6120
      Picture         =   "frmGame.frx":2894C
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   40
      Left            =   7140
      Picture         =   "frmGame.frx":28DB5
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   39
      Left            =   8160
      Picture         =   "frmGame.frx":2921E
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   38
      Left            =   9180
      Picture         =   "frmGame.frx":29687
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   37
      Left            =   4080
      Picture         =   "frmGame.frx":29AF0
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   36
      Left            =   3060
      Picture         =   "frmGame.frx":29F59
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   35
      Left            =   2040
      Picture         =   "frmGame.frx":2A3C2
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   34
      Left            =   1020
      Picture         =   "frmGame.frx":2A82B
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   33
      Left            =   5100
      Picture         =   "frmGame.frx":2AC94
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   32
      Left            =   3060
      Picture         =   "frmGame.frx":2B0FD
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   31
      Left            =   1020
      Picture         =   "frmGame.frx":2B566
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   30
      Left            =   1020
      Picture         =   "frmGame.frx":2B9CF
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   29
      Left            =   8160
      Picture         =   "frmGame.frx":2BE38
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   28
      Left            =   0
      Picture         =   "frmGame.frx":2C2A1
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   27
      Left            =   0
      Picture         =   "frmGame.frx":2C70A
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   26
      Left            =   4080
      Picture         =   "frmGame.frx":2CB73
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   25
      Left            =   0
      Picture         =   "frmGame.frx":2CFDC
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   24
      Left            =   7140
      Picture         =   "frmGame.frx":2D445
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   23
      Left            =   9180
      Picture         =   "frmGame.frx":2D8AE
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   22
      Left            =   5100
      Picture         =   "frmGame.frx":2DD17
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   21
      Left            =   6120
      Picture         =   "frmGame.frx":2E180
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   20
      Left            =   8160
      Picture         =   "frmGame.frx":2E5E9
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   19
      Left            =   9180
      Picture         =   "frmGame.frx":2EA52
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   18
      Left            =   1020
      Picture         =   "frmGame.frx":2EEBB
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   17
      Left            =   3060
      Picture         =   "frmGame.frx":2F324
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   16
      Left            =   0
      Picture         =   "frmGame.frx":2F78D
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   15
      Left            =   2040
      Picture         =   "frmGame.frx":2FBF6
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   14
      Left            =   6120
      Picture         =   "frmGame.frx":3005F
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   13
      Left            =   4080
      Picture         =   "frmGame.frx":304C8
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   12
      Left            =   5100
      Picture         =   "frmGame.frx":30931
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   11
      Left            =   2040
      Picture         =   "frmGame.frx":30D9A
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   10
      Left            =   6120
      Picture         =   "frmGame.frx":31203
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   9
      Left            =   2040
      Picture         =   "frmGame.frx":3166C
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   8
      Left            =   9180
      Picture         =   "frmGame.frx":31AD5
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   7
      Left            =   7140
      Picture         =   "frmGame.frx":31F3E
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   6
      Left            =   8160
      Picture         =   "frmGame.frx":323A7
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   5
      Left            =   5100
      Picture         =   "frmGame.frx":32810
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   4
      Left            =   4080
      Picture         =   "frmGame.frx":32C79
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   3
      Left            =   9180
      Picture         =   "frmGame.frx":330E2
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   2
      Left            =   2040
      Picture         =   "frmGame.frx":3354B
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   1
      Left            =   1020
      Picture         =   "frmGame.frx":339B4
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      Caption         =   "Left Click For Next Life!"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   15.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   495
      Left            =   2940
      TabIndex        =   8
      Top             =   5400
      Visible         =   0   'False
      Width           =   4935
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      Caption         =   "Left Click to Start!"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   15.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   495
      Left            =   3240
      TabIndex        =   7
      Top             =   5340
      Width           =   4335
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackColor       =   &H00000000&
      Caption         =   "Game Paused - Right Click"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   15.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   495
      Left            =   2820
      TabIndex        =   6
      Top             =   5400
      Visible         =   0   'False
      Width           =   5175
   End
   Begin VB.Label bricksleft 
      BackColor       =   &H00000000&
      Caption         =   "Bricks: x"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   2100
      TabIndex        =   5
      Top             =   0
      Width           =   1095
   End
   Begin VB.Label scorecard 
      BackColor       =   &H00000000&
      Caption         =   "Score: 0"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   3900
      TabIndex        =   4
      Top             =   0
      Width           =   1695
   End
   Begin VB.Label livesleft 
      BackColor       =   &H00000000&
      Caption         =   "Lives: 2"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   5700
      TabIndex        =   3
      Top             =   0
      Width           =   1095
   End
   Begin VB.Label Label4 
      BackColor       =   &H00000000&
      Caption         =   "Level: 1"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7500
      TabIndex        =   2
      Top             =   0
      Width           =   1095
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   0
      Left            =   6120
      Picture         =   "frmGame.frx":33E1D
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image Image1 
      Height          =   7995
      Left            =   0
      MouseIcon       =   "frmGame.frx":34286
      MousePointer    =   99  'Custom
      Top             =   0
      Width           =   10230
   End
End
Attribute VB_Name = "frmGame"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim bricks As Integer
Dim lives As Integer
Dim score As Integer
Dim level As Integer
Dim points As Integer
Dim a, MovUD, MovLR
Dim ballspeed As Integer
Private FalHang As String
Private UtoHang As String
Private KiHang As String
Private TeglaHang As String
Private FelVeszHang As String
Private MoreLevel As Boolean
Private XL As Single
Private Declare Function sndPlaySound Lib "winmm.dll" Alias "sndPlaySoundA" (ByVal lpszSoundName As String, ByVal uFlags As Long) As Long
Private IX() As Boolean
Private N1 As Single, N2 As Single, N3 As Single
Private labdaType As Single

Private Sub ball_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
    If Label1.Visible = True Or Label2.Visible = True Then
        Label1.Visible = False
        Label2.Visible = False
        Timer1.Enabled = True
    Else: Exit Sub
    End If
End If
End Sub





Private Sub bouncer_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
    If Label1.Visible = True Or Label2.Visible = True Then
        Label1.Visible = False
        Label2.Visible = False
        Timer1.Enabled = True
    Else: Exit Sub
    End If
End If
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)

'If KeyCode = vbKeyEscape Then
'    If Timer1.Enabled = False Then
'
'        ReleaseCursor Me
'    Else: Exit Sub
'    End If
'End If
End Sub
Public Sub Form_Load()
If App.PrevInstance = True Then End
ballspeed = 60
XL = 1
Dim II As Single
OpenLevel 1
ReDim IX(39)
For II = 0 To 38
IX(II) = False
Next
For II = 0 To 38
I(II).Top = block(II).Top
I(II).Left = block(II).Left
I(II).Visible = False
Next

Dim u As Single
For II = 0 To 139
If block(II).Visible = True Then
If block(II).Tag = 1 Or block(II).Tag = 3 Then
u = u + 1
End If
End If
Next
    bricks = u
    bricksleft.Caption = "Bricks: " & bricks
    level = 1
    Label4.Caption = "Level: " & level
    score = 0
    scorecard.Caption = "Score: " & score
    lives = 4
    livesleft.Caption = "Lives: " & lives
    Label1.Visible = True
    bouncer.Left = 3600
    bouncer.Visible = True
    ball.Top = bouncer.Top - bouncer.Height + 50
    ball.Left = bouncer.Left - ball.Width / 2 + 770
    ball.Visible = True
    MovUD = -(ballspeed)
    MovLR = (ballspeed)

FalHang = App.Path & "\snd\fal.wav"
UtoHang = App.Path & "\snd\uto.wav"
TeglaHang = App.Path & "\snd\tegla.wav"
KiHang = App.Path & "\snd\ki.wav"
FelVeszHang = App.Path & "\snd\bing.wav"
ball.Picture = frmTemp.L1.Picture 'rendes
labdaType = 1
bouncer.Tag = 1

FormOnTop Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
If Timer1.Enabled = True Then Cancel = 1
    ReleaseCursor Me
End Sub

Private Sub Form_Unload(Cancel As Integer)
ReleaseCursor Me
Timer1.Enabled = False
On Error Resume Next
Unload frmTemp
End Sub

Private Sub Image1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
    If Label1.Visible = True Or Label2.Visible = True Then
        Label1.Visible = False
        Label2.Visible = False
        Timer1.Enabled = True
    Else: Exit Sub
    End If
End If
If Button = 2 Then
    If Timer1.Enabled = True Then
        Timer1.Enabled = False
        Label3.Visible = True
    Exit Sub
    End If
    If Label3.Visible = True Then
        Timer1.Enabled = True
        Label3.Visible = False
    End If

End If
End Sub

Private Sub image1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Label3.Visible = True Then Exit Sub
bouncer.Left = X - bouncer.Width / 2
If Timer1.Enabled = False Then
ball.Left = X - ball.Width / 2
End If
End Sub
Private Sub bouncer_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If ball.Tag = "" Then Exit Sub
MoveCursor ball.Tag, 300

End Sub

Public Sub Timer1_Timer()
    MovUD = -(ballspeed)
    MovLR = (ballspeed)

Do
DoEvents
TrapCursor Me
ball.Top = ball.Top + MovUD
ball.Left = ball.Left + MovLR
For a = 0 To 139
'check bottom of the block
If MovUD < 0 Then
If block(a).Visible = True And ball.Top <= (block(a).Top + block(a).Height) And ball.Top >= block(a).Top And (ball.Left + ball.Width / 2) <= (block(a).Left + block(a).Width) And (ball.Left + ball.Width / 2) >= block(a).Left Then
If labdaType <> 2 Then
    MovUD = (ballspeed)
End If
    Call blockhit(block(a))
    sndPlaySound TeglaHang, 1
End If
End If
'check top of the block
If MovUD > 0 Then
If block(a).Visible = True And (ball.Top + ball.Height) >= block(a).Top And (ball.Top + ball.Height) <= (block(a).Top + block(a).Height) And (ball.Left + ball.Width / 2) <= (block(a).Left + block(a).Width) And (ball.Left + ball.Width / 2) >= block(a).Left Then
If labdaType <> 2 Then
    MovUD = -(ballspeed)
End If
    Call blockhit(block(a))
    sndPlaySound TeglaHang, 1
End If
End If
'check left of the block
If MovLR > 0 Then
If block(a).Visible = True And (ball.Left + ball.Width) >= block(a).Left And (ball.Left + ball.Width) <= (block(a).Left + block(a).Width) And (ball.Top + ball.Height / 2) <= (block(a).Top + block(a).Height) And (ball.Top + ball.Height / 2) >= block(a).Top Then
If labdaType <> 2 Then
    MovLR = -(ballspeed)
    End If
    
    Call blockhit(block(a))
    sndPlaySound TeglaHang, 1
End If
End If
'check right of the block
If MovLR < 0 Then
If block(a).Visible = True And ball.Left <= (block(a).Left + block(a).Width) And ball.Left >= block(a).Left And (ball.Top + ball.Height / 2) <= (block(a).Top + block(a).Height) And (ball.Top + ball.Height / 2) >= block(a).Top Then
If labdaType <> 2 Then
    MovLR = (ballspeed)
    End If
    Call blockhit(block(a))
    sndPlaySound TeglaHang, 1
End If
End If
Next a
If ball.Top < 0 Then
MovUD = (ballspeed)
sndPlaySound FalHang, 1
End If
If ball.Left < 2 Then
MovLR = (ballspeed)
sndPlaySound FalHang, 1
End If
If ball.Left > frmGame.Width - ball.Width - 50 Then
MovLR = -(ballspeed)
sndPlaySound FalHang, 1
End If
If (ball.Top + ball.Height) > bouncer.Top - 10 And (ball.Left + ball.Width) > bouncer.Left And ball.Left + ball.Width / 2 = (bouncer.Left + bouncer.Width / 2) Then
    MovUD = -(ballspeed)
    
    MovLR = Oldalra
    sndPlaySound UtoHang, 1
End If

If (ball.Top + ball.Height) > bouncer.Top - 10 And (ball.Left + ball.Width) > bouncer.Left - 100 And ball.Left + ball.Width / 2 < (bouncer.Left + bouncer.Width / 4) Then
    MovUD = -(ballspeed)
   
    MovLR = Oldalra
    sndPlaySound UtoHang, 1
End If

If (ball.Top + ball.Height) > bouncer.Top - 10 And (ball.Left + ball.Width) > bouncer.Left - 100 And ball.Left + ball.Width / 2 < (bouncer.Left + bouncer.Width / 4 * 2) And ball.Left + ball.Width / 2 > (bouncer.Left + bouncer.Width / 4) Then
    MovUD = -(ballspeed)
    
    MovLR = Oldalra
    sndPlaySound UtoHang, 1
End If

If (ball.Top + ball.Height) > bouncer.Top - 10 And ball.Left < (bouncer.Left + bouncer.Width) - 100 And ball.Left + ball.Width / 2 < (bouncer.Left + bouncer.Width / 4 * 3) And ball.Left + ball.Width / 2 > (bouncer.Left + bouncer.Width / 4 * 2) Then
    MovUD = -(ballspeed)
    
    MovLR = Oldalra
    sndPlaySound UtoHang, 1
End If

If (ball.Top + ball.Height) > bouncer.Top - 10 And ball.Left < (bouncer.Left + bouncer.Width) - 100 And ball.Left + ball.Width / 2 < (bouncer.Left + bouncer.Width / 4 * 4) And ball.Left + ball.Width / 2 > (bouncer.Left + bouncer.Width / 4 * 3) Then
    MovUD = -(ballspeed)
    MovLR = (ballspeed) + 10
    sndPlaySound UtoHang, 1
End If
If (ball.Top + ball.Height) > bouncer.Top + 300 Then
If Fal.Visible = False Then
deathhascome
sndPlaySound KiHang, 1
Else
sndPlaySound TeglaHang, 1
    MovUD = -(ballspeed)
End If
End If
If lives = -1 Then: endofgame: Exit Do
If bricks = 0 Then: leveldone: Exit Do
ReleaseCursor Me
Loop Until Timer1.Enabled = False
End Sub
Public Sub blockhit(block As Object)


If block.Tag = 1 Then
If labdaType <> 3 Then
For II = 0 To 38
If I(II).Top = block.Top Then
If I(II).Left = block.Left Then
IX(II) = True
End If
End If
Next II
block.Visible = False
    bricks = bricks - 1
    bricksleft.Caption = "Bricks: " & bricks
    score = score + 5
    scorecard.Caption = "Score: " & score
End If
ElseIf block.Tag = 2 Then
If labdaType = 2 Then
For II = 0 To 38
If I(II).Top = block.Top Then
If I(II).Left = block.Left Then
IX(II) = True
End If
End If
Next II
    block.Visible = False
End If
ElseIf block.Tag = 3 Then
If labdaType <> 3 Then
SetTeglaType block.Index, 4
End If
ElseIf block.Tag = 4 Then
If labdaType <> 3 Then
For II = 0 To 38
If I(II).Top = block.Top Then
If I(II).Left = block.Left Then
IX(II) = True
End If
End If
Next II
block.Visible = False
    bricks = bricks - 1
    bricksleft.Caption = "Bricks: " & bricks
    score = score + 5
    scorecard.Caption = "Score: " & score
End If
End If
    
End Sub
Public Sub endofgame()
    
    MsgBox "End game.", vbInformation, "BrickBreak"
    
    livesleft.Caption = "Lives: 0"
        frmGame.Enabled = False
ScoreX = score
    Timer1.Enabled = False
    Label2.Visible = False
    ReleaseCursor Me
       tempscores.Caption = score
    checkhighscore
    
   
    frmEndgame.Show 1
     Unload Me
End Sub
Public Sub leveldone()
    Timer1.Enabled = False
    ball.Visible = False
    score = score + 50

    scorecard.Caption = "Score: " & score
    level = level + 1
    Label4.Caption = "Level: " & level
    ball.Top = bouncer.Top - bouncer.Height + 50
    ball.Left = bouncer.Left - ball.Width / 2 + 770
    Label2.Visible = False
    Label3.Visible = False
    Label1.Visible = True
    MovUD = -(ballspeed)
    MovLR = (ballspeed)
    ball.Visible = True
    ResetPoints
    XL = XL + 1
    OpenLevel XL
        Dim u As Integer
For II = 0 To 139
If block(II).Visible = True Then
If block(II).Tag = 1 Or block(II).Tag = 3 Then
u = u + 1
End If
End If
Next
    bricks = u
    ballspeed = 60

    bricksleft.Caption = "Bricks: " & bricks

If MoreLevel = False Then
    tempscores.Caption = score
    ScoreX = score
    checkhighscore
    Timer1.Enabled = False
    Label2.Visible = False
    lives = 0
    livesleft.Caption = "Lives: " & lives
    ReleaseCursor Me
    ResetPoints
    frmGame.Enabled = False
    MsgBox "Congratulations, no more level. End game", vbInformation
   
    
    frmEndgame.Show 1
     Unload Me
End If
End Sub
Public Sub deathhascome()
    Label2.Visible = True
    Timer1.Enabled = False
    scorecard.Caption = "Score: " & score
    ScoreX = score
    lives = lives - 1
    livesleft.Caption = "Lives: " & lives
    ball.Top = bouncer.Top - bouncer.Height + 50
    ball.Left = bouncer.Left - ball.Width / 2 + 770
    MovUD = -(ballspeed)
    MovLR = (ballspeed)
    ball.Picture = frmTemp.L1.Picture 'rendes
labdaType = 1
Fal.Visible = False
TT3.Enabled = False
TT1.Enabled = False
TT2.Enabled = False
bouncer.Picture = frmTemp.U1.Picture 'rendes
bouncer.Tag = 1

End Sub
Public Sub changebrickcount()
'If level = 4 Then
'bricks = 48
'bricksleft.Caption = "Bricks: " & bricks
'End If
End Sub

Private Sub Timer2_Timer()
    GetCursorPosition
    ball.Tag = XPos
End Sub

Private Sub SetTeglaType(ByVal TeglaX As Integer, ByVal TypeT As Single)
Select Case TypeT
Case 1
block(TeglaX).Picture = frmTemp.T1.Picture
Case 2
block(TeglaX).Picture = frmTemp.T2.Picture
Case 3
block(TeglaX).Picture = frmTemp.T3.Picture
Case 4
block(TeglaX).Picture = frmTemp.T4.Picture
End Select
block(TeglaX).Tag = TypeT
End Sub

Private Sub OpenLevel(ByVal LevelYY As Single)
Dim Fs As String
Dim L(), LL()
Dim n As Single, m As Single
n = 0
m = 0
ReDim L(140)
ReDim LL(140)
FS2 = App.Path & "\level\" & GameName & "\" & LevelYY
If Dir(FS2) = "" Then GoTo L1
Open FS2 For Input As #1
Do While Not EOF(1)
If n <= 139 Then
Line Input #1, L(n)
n = n + 1
Else
Line Input #1, LL(m)
m = m + 1
End If
Loop
Close #1
For II = 0 To 139
block(II).Visible = L(II)
Next
For II = 0 To 139
SetTeglaType II, LL(II)
Next
MoreLevel = True
Exit Sub
L1:
For II = 0 To 139
block(II).Visible = False
Next
MoreLevel = False
End Sub

Private Function Oldalra() As Single
If ball.Left > bouncer.Left + (bouncer.Width / 2) Then
Oldalra = (ball.Left - (bouncer.Left + (bouncer.Width / 2))) / 5
ElseIf ball.Left < bouncer.Left + (bouncer.Width / 2) Then
Oldalra = (ball.Left - (bouncer.Left + (bouncer.Width / 2))) / 5
Else
Oldalra = 0
End If
End Function

Public Sub checkhighscore()
Dim score1, score2, score3, score4, score5, score6, score7, score8, score9, score10, tempscore As Integer
Call WriteINI("Scores", "TempScore", tempscores.Caption, App.Path & "\brix.ini")
score10 = ReadINI("Scores", "Score10", App.Path & "\brix.ini")
score9 = ReadINI("Scores", "Score9", App.Path & "\brix.ini")
score8 = ReadINI("Scores", "Score8", App.Path & "\brix.ini")
score7 = ReadINI("Scores", "Score7", App.Path & "\brix.ini")
score6 = ReadINI("Scores", "Score6", App.Path & "\brix.ini")
score5 = ReadINI("Scores", "Score5", App.Path & "\brix.ini")
score4 = ReadINI("Scores", "Score4", App.Path & "\brix.ini")
score3 = ReadINI("Scores", "Score3", App.Path & "\brix.ini")
score2 = ReadINI("Scores", "Score2", App.Path & "\brix.ini")
score1 = ReadINI("Scores", "Score1", App.Path & "\brix.ini")
tempscore = ReadINI("Scores", "Tempscore", App.Path & "\brix.ini")
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
tempscores.Caption = tempscore
ScoreX = tempscore
If tempscore = 0 Then Exit Sub
If tempscore < 0 Then Exit Sub

If tempscore >= score1 Then
Me.Tag = "score1"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score4", scores3.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score3", scores2.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score2", scores1.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score1", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score2 Then
Me.Tag = "score2"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score4", scores3.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score3", scores2.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score2", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score3 Then
Me.Tag = "score3"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score4", scores3.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score3", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score4 Then
Me.Tag = "score4"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score5", scores4.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score4", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If
If tempscore >= score5 Then
Me.Tag = "score5"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", scores5.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score5", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score6 Then
Me.Tag = "score6"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", scores6.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score6", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score7 Then
Me.Tag = "score7"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", scores7.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score7", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score8 Then
Me.Tag = "score8"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", scores8.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score8", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score9 Then
Me.Tag = "score9"
Call WriteINI("Scores", "Score10", scores9.Caption, App.Path & "\brix.ini")
Call WriteINI("Scores", "Score9", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If

If tempscore >= score10 Then
Me.Tag = "score10"
Call WriteINI("Scores", "Score10", tempscores.Caption, App.Path & "\brix.ini")
Exit Sub
End If
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

Private Sub Timer3_Timer()
For II = 0 To 38
If IX(II) = True Then
I(II).Visible = True
I(II).Top = I(II).Top + 20
End If
Next

For II = 0 To 38
    If IX(II) = True Then
        If I(II).Left >= bouncer.Left And I(II).Left <= bouncer.Left + bouncer.Width Then
            If I(II).Top >= bouncer.Top - bouncer.Height - 10 And I(II).Top <= bouncer.Top Then
            sndPlaySound FelVeszHang, 1
            IX(II) = False
            I(II).Visible = False
                If I(II).Tag = 2 Then
                TT1.Enabled = True
                N1 = 0
                SetLabdaType 2
                score = score + 10
                scorecard.Caption = "Score: " & score
                ElseIf I(II).Tag = 3 Then
                TT2.Enabled = True
                N2 = 0
                score = score + 10
                scorecard.Caption = "Score: " & score
                SetLabdaType 3
                ElseIf I(II).Tag = 8 Then
                TT3.Enabled = True
                Fal.Visible = True
                N3 = 0
                score = score + 10
                scorecard.Caption = "Score: " & score
                End If
                If I(II).Tag = 1 Then
                score = score + 100
                scorecard.Caption = "Score: " & score
                End If
                If I(II).Tag = 9 Then
                lives = lives + 1
                livesleft.Caption = "Lives: " & lives
                End If
                If I(II).Tag = 6 Then
                    If bouncer.Tag = 1 Then
                    SetUto 2
                    ElseIf bouncer.Tag = 3 Then
                    SetUto 1
                    End If
                End If
                If I(II).Tag = 7 Then
                    If bouncer.Tag = 2 Then
                    SetUto 1
                    ElseIf bouncer.Tag = 1 Then
                    SetUto 3
                    End If
                End If
                If I(II).Tag = 4 Then
                If ballspeed < 100 Then
                ballspeed = ballspeed + 20
                End If
                End If
                If I(II).Tag = 5 Then
                If ballspeed > 20 Then
                ballspeed = ballspeed - 20
                End If
                End If
            End If
        End If
    End If
Next
End Sub

Private Sub SetLabdaType(ByVal TypeLabda As Single)
Select Case TypeLabda
Case 1
ball.Picture = frmTemp.L1.Picture 'rendes
labdaType = 1
Case 2
ball.Picture = frmTemp.L3.Picture 'kek
labdaType = 2
Case 3
ball.Picture = frmTemp.L2.Picture 'piros
labdaType = 3
End Select
End Sub

Private Sub SetUto(ByVal TypeUto As Single)
Select Case TypeUto
Case 1
bouncer.Picture = frmTemp.U1.Picture 'rendes
bouncer.Tag = 1
Case 2
bouncer.Picture = frmTemp.U2.Picture 'hosszu
bouncer.Tag = 2
Case 3
bouncer.Picture = frmTemp.U3.Picture 'rovid
bouncer.Tag = 3
End Select
End Sub

Private Sub TT1_Timer()

If N1 = 10 Then
SetLabdaType 1
TT1.Enabled = False
Else
N1 = N1 + 1
End If
End Sub

Private Sub TT2_Timer()
If N2 = 10 Then
SetLabdaType 1
TT2.Enabled = False
Else
N2 = N2 + 1
End If

End Sub

Private Sub TT3_Timer()
If N3 = 10 Then
Fal.Visible = False
TT3.Enabled = False
Else
N3 = N3 + 1
End If
End Sub

Private Sub ResetPoints()
For II = 0 To 38
IX(II) = False
Next
For II = 0 To 38
I(II).Top = block(II).Top
I(II).Left = block(II).Left
I(II).Visible = False
Next
TT1.Enabled = False
TT2.Enabled = False
TT3.Enabled = False
N1 = 0: N2 = 0: N3 = 0
    ball.Picture = frmTemp.L1.Picture 'rendes
labdaType = 1
Fal.Visible = False
bouncer.Picture = frmTemp.U1.Picture 'rendes
bouncer.Tag = 1
ballspeed = 40
End Sub
