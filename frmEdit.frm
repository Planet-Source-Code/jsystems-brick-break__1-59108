VERSION 5.00
Begin VB.Form frmEdit 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Level editor"
   ClientHeight    =   8595
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   10365
   Icon            =   "frmEdit.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8595
   ScaleWidth      =   10365
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox T1 
      AutoSize        =   -1  'True
      Height          =   360
      Left            =   120
      Picture         =   "frmEdit.frx":1CFA
      ScaleHeight     =   300
      ScaleWidth      =   1050
      TabIndex        =   3
      Top             =   6240
      Width           =   1110
   End
   Begin VB.PictureBox T2 
      AutoSize        =   -1  'True
      Height          =   360
      Left            =   120
      Picture         =   "frmEdit.frx":2163
      ScaleHeight     =   300
      ScaleWidth      =   1050
      TabIndex        =   2
      Top             =   6600
      Width           =   1110
   End
   Begin VB.PictureBox T3 
      AutoSize        =   -1  'True
      Height          =   360
      Left            =   120
      Picture         =   "frmEdit.frx":27AD
      ScaleHeight     =   300
      ScaleWidth      =   1050
      TabIndex        =   1
      Top             =   6960
      Width           =   1110
   End
   Begin VB.CommandButton Command1 
      Caption         =   "SAVE LEVEL"
      Height          =   315
      Left            =   4740
      TabIndex        =   0
      Top             =   8220
      Width           =   1395
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   139
      Left            =   8280
      Picture         =   "frmEdit.frx":2D8A
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   138
      Left            =   9300
      Picture         =   "frmEdit.frx":31F3
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   137
      Left            =   6240
      Picture         =   "frmEdit.frx":365C
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   136
      Left            =   2160
      Picture         =   "frmEdit.frx":3AC5
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   135
      Left            =   5220
      Picture         =   "frmEdit.frx":3F2E
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   134
      Left            =   1140
      Picture         =   "frmEdit.frx":4397
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   133
      Left            =   1140
      Picture         =   "frmEdit.frx":4800
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   132
      Left            =   2160
      Picture         =   "frmEdit.frx":4C69
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   131
      Left            =   3180
      Picture         =   "frmEdit.frx":50D2
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   130
      Left            =   4200
      Picture         =   "frmEdit.frx":553B
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   129
      Left            =   5220
      Picture         =   "frmEdit.frx":59A4
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   128
      Left            =   6240
      Picture         =   "frmEdit.frx":5E0D
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   127
      Left            =   7260
      Picture         =   "frmEdit.frx":6276
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   126
      Left            =   8280
      Picture         =   "frmEdit.frx":66DF
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   125
      Left            =   9300
      Picture         =   "frmEdit.frx":6B48
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   124
      Left            =   7260
      Picture         =   "frmEdit.frx":6FB1
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   123
      Left            =   4200
      Picture         =   "frmEdit.frx":741A
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   122
      Left            =   3180
      Picture         =   "frmEdit.frx":7883
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   121
      Left            =   2160
      Picture         =   "frmEdit.frx":7CEC
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   120
      Left            =   5220
      Picture         =   "frmEdit.frx":8155
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   119
      Left            =   5220
      Picture         =   "frmEdit.frx":85BE
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   118
      Left            =   3180
      Picture         =   "frmEdit.frx":8A27
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   117
      Left            =   1140
      Picture         =   "frmEdit.frx":8E90
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   116
      Left            =   120
      Picture         =   "frmEdit.frx":92F9
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   115
      Left            =   120
      Picture         =   "frmEdit.frx":9762
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   114
      Left            =   4200
      Picture         =   "frmEdit.frx":9BCB
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   113
      Left            =   120
      Picture         =   "frmEdit.frx":A034
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   112
      Left            =   7260
      Picture         =   "frmEdit.frx":A49D
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   111
      Left            =   9300
      Picture         =   "frmEdit.frx":A906
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   110
      Left            =   6240
      Picture         =   "frmEdit.frx":AD6F
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   109
      Left            =   8280
      Picture         =   "frmEdit.frx":B1D8
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   108
      Left            =   1140
      Picture         =   "frmEdit.frx":B641
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   107
      Left            =   3180
      Picture         =   "frmEdit.frx":BAAA
      Top             =   3900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   106
      Left            =   120
      Picture         =   "frmEdit.frx":BF13
      Top             =   4200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   105
      Left            =   4200
      Picture         =   "frmEdit.frx":C37C
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   104
      Left            =   2160
      Picture         =   "frmEdit.frx":C7E5
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   103
      Left            =   6240
      Picture         =   "frmEdit.frx":CC4E
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   102
      Left            =   7260
      Picture         =   "frmEdit.frx":D0B7
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   101
      Left            =   9300
      Picture         =   "frmEdit.frx":D520
      Top             =   3300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   100
      Left            =   8280
      Picture         =   "frmEdit.frx":D989
      Top             =   3600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   0
      Left            =   6240
      Picture         =   "frmEdit.frx":DDF2
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   1
      Left            =   1140
      Picture         =   "frmEdit.frx":E25B
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   2
      Left            =   2160
      Picture         =   "frmEdit.frx":E6C4
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   3
      Left            =   9300
      Picture         =   "frmEdit.frx":EB2D
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   4
      Left            =   4200
      Picture         =   "frmEdit.frx":EF96
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   5
      Left            =   5220
      Picture         =   "frmEdit.frx":F3FF
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   6
      Left            =   8280
      Picture         =   "frmEdit.frx":F868
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   7
      Left            =   7260
      Picture         =   "frmEdit.frx":FCD1
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   8
      Left            =   9300
      Picture         =   "frmEdit.frx":1013A
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   9
      Left            =   2160
      Picture         =   "frmEdit.frx":105A3
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   10
      Left            =   6240
      Picture         =   "frmEdit.frx":10A0C
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   11
      Left            =   2160
      Picture         =   "frmEdit.frx":10E75
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   12
      Left            =   5220
      Picture         =   "frmEdit.frx":112DE
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   13
      Left            =   4200
      Picture         =   "frmEdit.frx":11747
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   14
      Left            =   6240
      Picture         =   "frmEdit.frx":11BB0
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   15
      Left            =   2160
      Picture         =   "frmEdit.frx":12019
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   16
      Left            =   120
      Picture         =   "frmEdit.frx":12482
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   17
      Left            =   3180
      Picture         =   "frmEdit.frx":128EB
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   18
      Left            =   1140
      Picture         =   "frmEdit.frx":12D54
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   19
      Left            =   9300
      Picture         =   "frmEdit.frx":131BD
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   20
      Left            =   8280
      Picture         =   "frmEdit.frx":13626
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   21
      Left            =   6240
      Picture         =   "frmEdit.frx":13A8F
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   22
      Left            =   5220
      Picture         =   "frmEdit.frx":13EF8
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   23
      Left            =   9300
      Picture         =   "frmEdit.frx":14361
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   24
      Left            =   7260
      Picture         =   "frmEdit.frx":147CA
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   25
      Left            =   120
      Picture         =   "frmEdit.frx":14C33
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   26
      Left            =   4200
      Picture         =   "frmEdit.frx":1509C
      Top             =   1200
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   27
      Left            =   120
      Picture         =   "frmEdit.frx":15505
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   28
      Left            =   120
      Picture         =   "frmEdit.frx":1596E
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   29
      Left            =   8280
      Picture         =   "frmEdit.frx":15DD7
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   30
      Left            =   1140
      Picture         =   "frmEdit.frx":16240
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   31
      Left            =   1140
      Picture         =   "frmEdit.frx":166A9
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   32
      Left            =   3180
      Picture         =   "frmEdit.frx":16B12
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   33
      Left            =   5220
      Picture         =   "frmEdit.frx":16F7B
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   34
      Left            =   1140
      Picture         =   "frmEdit.frx":173E4
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   35
      Left            =   2160
      Picture         =   "frmEdit.frx":1784D
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   36
      Left            =   3180
      Picture         =   "frmEdit.frx":17CB6
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   37
      Left            =   4200
      Picture         =   "frmEdit.frx":1811F
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   38
      Left            =   9300
      Picture         =   "frmEdit.frx":18588
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   39
      Left            =   8280
      Picture         =   "frmEdit.frx":189F1
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   40
      Left            =   7260
      Picture         =   "frmEdit.frx":18E5A
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   41
      Left            =   6240
      Picture         =   "frmEdit.frx":192C3
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   42
      Left            =   1140
      Picture         =   "frmEdit.frx":1972C
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   43
      Left            =   8280
      Picture         =   "frmEdit.frx":19B95
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   44
      Left            =   7260
      Picture         =   "frmEdit.frx":19FFE
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   45
      Left            =   6240
      Picture         =   "frmEdit.frx":1A467
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   46
      Left            =   5220
      Picture         =   "frmEdit.frx":1A8D0
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   47
      Left            =   3180
      Picture         =   "frmEdit.frx":1AD39
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   48
      Left            =   3180
      Picture         =   "frmEdit.frx":1B1A2
      Top             =   1500
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   49
      Left            =   8280
      Picture         =   "frmEdit.frx":1B60B
      Top             =   900
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   50
      Left            =   2160
      Picture         =   "frmEdit.frx":1BA74
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   51
      Left            =   3180
      Picture         =   "frmEdit.frx":1BEDD
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   52
      Left            =   4200
      Picture         =   "frmEdit.frx":1C346
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   53
      Left            =   5220
      Picture         =   "frmEdit.frx":1C7AF
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   54
      Left            =   6240
      Picture         =   "frmEdit.frx":1CC18
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   55
      Left            =   7260
      Picture         =   "frmEdit.frx":1D081
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   56
      Left            =   8280
      Picture         =   "frmEdit.frx":1D4EA
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   57
      Left            =   9300
      Picture         =   "frmEdit.frx":1D953
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   58
      Left            =   5220
      Picture         =   "frmEdit.frx":1DDBC
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   59
      Left            =   7260
      Picture         =   "frmEdit.frx":1E225
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   60
      Left            =   1140
      Picture         =   "frmEdit.frx":1E68E
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   61
      Left            =   9300
      Picture         =   "frmEdit.frx":1EAF7
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   62
      Left            =   4200
      Picture         =   "frmEdit.frx":1EF60
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   63
      Left            =   3180
      Picture         =   "frmEdit.frx":1F3C9
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   64
      Left            =   2160
      Picture         =   "frmEdit.frx":1F832
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   65
      Left            =   5220
      Picture         =   "frmEdit.frx":1FC9B
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   66
      Left            =   5220
      Picture         =   "frmEdit.frx":20104
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   67
      Left            =   3180
      Picture         =   "frmEdit.frx":2056D
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   68
      Left            =   120
      Picture         =   "frmEdit.frx":209D6
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   69
      Left            =   1140
      Picture         =   "frmEdit.frx":20E3F
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   70
      Left            =   8280
      Picture         =   "frmEdit.frx":212A8
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   71
      Left            =   120
      Picture         =   "frmEdit.frx":21711
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   72
      Left            =   120
      Picture         =   "frmEdit.frx":21B7A
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   73
      Left            =   4200
      Picture         =   "frmEdit.frx":21FE3
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   74
      Left            =   120
      Picture         =   "frmEdit.frx":2244C
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   75
      Left            =   7260
      Picture         =   "frmEdit.frx":228B5
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   76
      Left            =   9300
      Picture         =   "frmEdit.frx":22D1E
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   77
      Left            =   4200
      Picture         =   "frmEdit.frx":23187
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   78
      Left            =   6240
      Picture         =   "frmEdit.frx":235F0
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   79
      Left            =   8280
      Picture         =   "frmEdit.frx":23A59
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   80
      Left            =   9300
      Picture         =   "frmEdit.frx":23EC2
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   81
      Left            =   1140
      Picture         =   "frmEdit.frx":2432B
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   82
      Left            =   3180
      Picture         =   "frmEdit.frx":24794
      Top             =   2700
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   83
      Left            =   120
      Picture         =   "frmEdit.frx":24BFD
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   84
      Left            =   2160
      Picture         =   "frmEdit.frx":25066
      Top             =   600
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   85
      Left            =   6240
      Picture         =   "frmEdit.frx":254CF
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   86
      Left            =   4200
      Picture         =   "frmEdit.frx":25938
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   87
      Left            =   2160
      Picture         =   "frmEdit.frx":25DA1
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   88
      Left            =   7260
      Picture         =   "frmEdit.frx":2620A
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   89
      Left            =   4200
      Picture         =   "frmEdit.frx":26673
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   90
      Left            =   3180
      Picture         =   "frmEdit.frx":26ADC
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   91
      Left            =   6240
      Picture         =   "frmEdit.frx":26F45
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   92
      Left            =   7260
      Picture         =   "frmEdit.frx":273AE
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   93
      Left            =   7260
      Picture         =   "frmEdit.frx":27817
      Top             =   300
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   94
      Left            =   9300
      Picture         =   "frmEdit.frx":27C80
      Top             =   2100
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   95
      Left            =   5220
      Picture         =   "frmEdit.frx":280E9
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   96
      Left            =   8280
      Picture         =   "frmEdit.frx":28552
      Top             =   2400
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   97
      Left            =   2160
      Picture         =   "frmEdit.frx":289BB
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   98
      Left            =   1140
      Picture         =   "frmEdit.frx":28E24
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image block 
      Height          =   300
      Index           =   99
      Left            =   120
      Picture         =   "frmEdit.frx":2928D
      Top             =   1800
      Width           =   1050
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   0
      Left            =   6240
      Picture         =   "frmEdit.frx":296F6
      Tag             =   "1"
      Top             =   1500
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   1
      Left            =   1140
      Picture         =   "frmEdit.frx":29A00
      Tag             =   "9"
      Top             =   1200
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   2
      Left            =   2160
      Picture         =   "frmEdit.frx":29D0A
      Tag             =   "9"
      Top             =   300
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   3
      Left            =   9240
      Picture         =   "frmEdit.frx":2A014
      Tag             =   "6"
      Top             =   1500
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   4
      Left            =   4200
      Picture         =   "frmEdit.frx":2A31E
      Tag             =   "2"
      Top             =   1500
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   5
      Left            =   5220
      Picture         =   "frmEdit.frx":2A628
      Tag             =   "4"
      Top             =   900
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   6
      Left            =   8280
      Picture         =   "frmEdit.frx":2A932
      Tag             =   "5"
      Top             =   2100
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   7
      Left            =   7260
      Picture         =   "frmEdit.frx":2AC3C
      Tag             =   "7"
      Top             =   600
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   8
      Left            =   9240
      Picture         =   "frmEdit.frx":2AF46
      Tag             =   "3"
      Top             =   2700
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   9
      Left            =   2160
      Picture         =   "frmEdit.frx":2B250
      Tag             =   "7"
      Top             =   1500
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   10
      Left            =   6240
      Picture         =   "frmEdit.frx":2B55A
      Tag             =   "8"
      Top             =   2400
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   11
      Left            =   2160
      Picture         =   "frmEdit.frx":2B864
      Tag             =   "1"
      Top             =   2700
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Image I 
      Height          =   480
      Index           =   12
      Left            =   5220
      Picture         =   "frmEdit.frx":2BB6E
      Tag             =   "6"
      Top             =   2700
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      X1              =   120
      X2              =   10320
      Y1              =   4500
      Y2              =   4500
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00FFFFFF&
      X1              =   120
      X2              =   10320
      Y1              =   300
      Y2              =   300
   End
End
Attribute VB_Name = "frmEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal crKey As Long, ByVal bAlpha As Byte, ByVal dwFlags As Long) As Long
Private Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long

Private Const GWL_EXSTYLE = (-20)
Private Const LWA_ALPHA = &H2
Private Const WS_EX_LAYERED = &H80000
Private TeglaXT As Single
Private II As Long

Private Function Transparent(ByVal hWnd As Long, Perc As Integer) As Long
    Dim Msg As Long
    On Error Resume Next
    If Perc < 0 Or Perc > 255 Then
      Transparent = 1
    Else
      Msg = GetWindowLong(hWnd, GWL_EXSTYLE)
      Msg = Msg Or WS_EX_LAYERED
      SetWindowLong hWnd, GWL_EXSTYLE, Msg
      SetLayeredWindowAttributes hWnd, 0, Perc, LWA_ALPHA
      Transparent = 0
    End If
    If Err Then
      Transparent = 2
    End If
End Function


Private Sub block_Click(Index As Integer)
block(Index).Visible = False
End Sub

Private Sub Command1_Click()
SaveLevel
Unload Me
frmPreEdit.Show
End Sub

Private Sub Form_Load()
For II = 0 To 139
block(II).Visible = False
block(II).Tag = 1
Next
'Transparent Picture1.hWnd, 150 'max 250-rendes

OpenLevel
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
For II = 0 To 139
If X > block(II).Left And X < block(II).Left + block(II).Width Then
If Y > block(II).Top And Y < block(II).Top + block(II).Height Then
block(II).Visible = True
SetTeglaType II, TeglaXT
End If
End If
Next
End If
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

Private Sub SaveLevel()
If GameName = "" Then GameName = "demo"
If LevelX = 0 Then LevelX = 1
Dim Fs As String
Dim FS2 As String
Fs = App.Path & "\level\" & GameName

FS2 = App.Path & "\level\" & GameName & "\" & LevelX
Open FS2 For Output As #1
For II = 0 To 139
Print #1, block(II).Visible
Next
For II = 0 To 139
If block(II).Tag = 0 Then block(II).Tag = 1
Print #1, block(II).Tag
Next
Close #1
End Sub

Private Sub OpenLevel()
Dim Fs As String
Dim L(), LL()
Dim n As Single, m As Single
n = 0
m = 0
ReDim L(140)
ReDim LL(140)
FS2 = App.Path & "\level\" & GameName & "\" & LevelX
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
Exit Sub
L1:
For II = 0 To 139
block(II).Visible = False
Next
For II = 0 To 139
SetTeglaType II, 1
Next
End Sub

Private Sub Form_Unload(Cancel As Integer)
frmPreEdit.Show
End Sub

Private Sub T1_Click()
TeglaXT = 1
End Sub

Private Sub T2_Click()
TeglaXT = 2
End Sub

Private Sub T3_Click()
TeglaXT = 3
End Sub
