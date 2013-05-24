#!/usr/bin/env python3

#
# TODO:
#
# * Passing directory path (Done)
#
# * Document for PySide:
#   - Setting QPushButton with pixmap (Done)
#   - Using action (Done)
#   - Using shortcut (Done)
#   - Setting the application icon (Done)
#   - How to construct context menu (Done)
#   - closeEvent (Done)
#
# * Last visited dir @ `$HOME/emacs-config/filebrowser-lastdir.txt` (Done)
#
# * Autocomplete path
#
# * Up button
#
# * Back button
#
# * Expand/collapse all button
#
# * Setting the application icon (Done)
#
# * Shortcuts:
#   - <Ctrl L> to jump to path entry (Done)
#   - <Ctrl O> to browse directory graphically (Done)
#   - <Ctrl F> to jump to filter entry (Done)
#
# * Right-click context menu:
#   - Go to dir
#   - New dir
#   - Copy full path (Done)
#   - Copy file name (Done)
#   - Delete file (Done)
#
# * File filter (Done)
#
# * Expand path (Done)
#


import sys
import os
import re

from PySide.QtCore import *
from PySide.QtGui import *


DEFAULT_ROOT_PATH = "~/"
CONFIG_FILE = "~/emacs-config/filebrowser-lastdir.txt"


def getOpenDirIcon():
    return QIcon(QPixmap([
        "32 32 471 2",
        "   c None",
        ".  c #8CA7FE",
        "+  c #6484FE",
        "@  c #8BA6FD",
        "#  c #B7C9FC",
        "$  c #8DB3EC",
        "%  c #5B7BFD",
        "&  c #8AAAFF",
        "*  c #BACCFD",
        "=  c #A5CBE5",
        "-  c #99C5E0",
        ";  c #6387F5",
        ">  c #7E93FF",
        ",  c #AABCD0",
        "'  c #AEBFD2",
        ")  c #BBCDFD",
        "!  c #ACCFE8",
        "~  c #A0C9E2",
        "{  c #95C1E0",
        "]  c #84B2E1",
        "^  c #4160FC",
        "/  c #BFCCDC",
        "(  c #CFD9E4",
        "_  c #FFFFFF",
        ":  c #F7F9FB",
        "<  c #D1DBE6",
        "[  c #ADBFD2",
        "}  c #A7BACE",
        "|  c #AFC1D2",
        "1  c #BACDFE",
        "2  c #B4D3EB",
        "3  c #A6CDE4",
        "4  c #9CC4E1",
        "5  c #91BDE0",
        "6  c #85B4DE",
        "7  c #618BE9",
        "8  c #6078FD",
        "9  c #B2C3D5",
        "0  c #FEFEFE",
        "a  c #FCFDFE",
        "b  c #BBCCEF",
        "c  c #B4C9FB",
        "d  c #B2D0E8",
        "e  c #A2C8E1",
        "f  c #9FC6E2",
        "g  c #97C1E2",
        "h  c #8CB9E0",
        "i  c #81B0DE",
        "j  c #7AAADE",
        "k  c #4765F9",
        "l  c #C3D1DF",
        "m  c #DEE5ED",
        "n  c #DFE8FF",
        "o  c #B4CAFD",
        "p  c #C4DCF1",
        "q  c #B3D4E8",
        "r  c #A5CAE4",
        "s  c #91BCDD",
        "t  c #81AFD8",
        "u  c #77A7D8",
        "v  c #75A5DB",
        "w  c #79A7E0",
        "x  c #6D97E5",
        "y  c #4156FC",
        "z  c #BBCAD9",
        "A  c #E4ECFF",
        "B  c #AEC6FD",
        "C  c #CDE0F4",
        "D  c #BAD8EA",
        "E  c #B0D0E8",
        "F  c #A5C9E6",
        "G  c #99C1E4",
        "H  c #8FB9E3",
        "I  c #81ADDF",
        "J  c #6C9DD9",
        "K  c #6495D6",
        "L  c #6493D8",
        "M  c #5B7AF0",
        "N  c #6877FE",
        "O  c #C7D4E0",
        "P  c #EAEEF3",
        "Q  c #F4F8FF",
        "R  c #A7C2FE",
        "S  c #D4E5F6",
        "T  c #C1DBEC",
        "U  c #B6D5EA",
        "V  c #ACCDE8",
        "W  c #A1C5E6",
        "X  c #96BCE4",
        "Y  c #8BB4E2",
        "Z  c #80ABE1",
        "`  c #77A4E1",
        " . c #76A1E3",
        ".. c #6491DB",
        "+. c #7099E4",
        "@. c #4B5EFC",
        "#. c #C8D3DF",
        "$. c #DBE6FF",
        "%. c #CFDFFC",
        "&. c #C8DFEE",
        "*. c #BDD8EC",
        "=. c #B3D1EA",
        "-. c #A8CAE8",
        ";. c #9CC1E6",
        ">. c #91B8E4",
        ",. c #86B0E3",
        "'. c #7AA5E2",
        "). c #749DE3",
        "!. c #608CDA",
        "~. c #6E95E4",
        "{. c #6C8EEB",
        "]. c #3E4DFD",
        "^. c #CCD7E4",
        "/. c #F3F5F8",
        "(. c #D3E1FF",
        "_. c #D0E0FC",
        ":. c #C4DDEE",
        "<. c #BAD5EC",
        "[. c #AFCEEA",
        "}. c #A3C5E8",
        "|. c #99BDE6",
        "1. c #8EB4E4",
        "2. c #82ABE4",
        "3. c #759EE4",
        "4. c #6690DE",
        "5. c #668EDD",
        "6. c #6D91E4",
        "7. c #7092EA",
        "8. c #607DEE",
        "9. c #3D46FD",
        "0. c #D3DCE7",
        "a. c #CCDCFF",
        "b. c #D0DFFB",
        "c. c #C0D9EE",
        "d. c #B6D2EB",
        "e. c #ABCAEA",
        "f. c #9FC1E7",
        "g. c #94B9E7",
        "h. c #88AFE5",
        "i. c #7DA5E4",
        "j. c #749EE4",
        "k. c #749BE5",
        "l. c #5C87DB",
        "m. c #5379E1",
        "n. c #6184E9",
        "o. c #6E8EEB",
        "p. c #6D8CEC",
        "q. c #5167F4",
        "r. c #4B50FE",
        "s. c #D2DCE6",
        "t. c #F9FAFC",
        "u. c #AFC8FF",
        "v. c #D1DFFA",
        "w. c #BCD6ED",
        "x. c #B1CEEC",
        "y. c #A6C6EA",
        "z. c #9CBEE8",
        "A. c #90B4E7",
        "B. c #84ABE6",
        "C. c #77A0E4",
        "D. c #6B92E3",
        "E. c #6089DE",
        "F. c #658DE5",
        "G. c #6083EB",
        "H. c #5574EC",
        "I. c #5D79EE",
        "J. c #4768ED",
        "K. c #3242F7",
        "L. c #5F60FF",
        "M. c #DFE6ED",
        "N. c #C8DAFF",
        "O. c #B6CBFD",
        "P. c #C4DAF1",
        "Q. c #B9D3ED",
        "R. c #ADCAEB",
        "S. c #A2C2EA",
        "T. c #97B9E8",
        "U. c #8BAFE7",
        "V. c #80A5E6",
        "W. c #7298E7",
        "X. c #6088DE",
        "Y. c #638AE3",
        "Z. c #678DEA",
        "`. c #678BED",
        " + c #6588EF",
        ".+ c #375DED",
        "++ c #1738EE",
        "@+ c #93A1F8",
        "#+ c #4D52FD",
        "$+ c #7170FF",
        "%+ c #D8E0E9",
        "&+ c #CCDDFF",
        "*+ c #B5CDFE",
        "=+ c #CCDFF3",
        "-+ c #BFD7EF",
        ";+ c #B5CFEC",
        ">+ c #A9C7EC",
        ",+ c #9EBEEA",
        "'+ c #92B4E9",
        ")+ c #86ABE8",
        "!+ c #7BA0E7",
        "~+ c #6E92E7",
        "{+ c #5F85E1",
        "]+ c #6389E6",
        "^+ c #6487EF",
        "/+ c #3F67ED",
        "(+ c #2E56EE",
        "_+ c #9DAEF8",
        ":+ c #DFE2FE",
        "<+ c #3938FE",
        "[+ c #7C76FF",
        "}+ c #E8EDF2",
        "|+ c #CFDFFF",
        "1+ c #B1CAFD",
        "2+ c #D3E4F6",
        "3+ c #C7DBF1",
        "4+ c #BCD3EF",
        "5+ c #B0CAED",
        "6+ c #A5C2EC",
        "7+ c #9AB9EA",
        "8+ c #8EAFEA",
        "9+ c #82A5E9",
        "0+ c #769BE8",
        "a+ c #7194E9",
        "b+ c #6288E3",
        "c+ c #6187E4",
        "d+ c #6286EA",
        "e+ c #4E75EC",
        "f+ c #355FEC",
        "g+ c #2D56EE",
        "h+ c #93A6F7",
        "i+ c #CED2FE",
        "j+ c #2219FF",
        "k+ c #DEE5EC",
        "l+ c #FEFFFF",
        "m+ c #D2E2FF",
        "n+ c #A7C5FD",
        "o+ c #CDDFF4",
        "p+ c #BDD6EE",
        "q+ c #B3CEED",
        "r+ c #AAC6EC",
        "s+ c #A3C1EB",
        "t+ c #9CBAEB",
        "u+ c #96B5EB",
        "v+ c #89ABEA",
        "w+ c #7D9FE9",
        "x+ c #6D92E8",
        "y+ c #678BE8",
        "z+ c #5179E1",
        "A+ c #446EE3",
        "B+ c #3360E9",
        "C+ c #2E5AEB",
        "D+ c #8A9FF7",
        "E+ c #7B7BFE",
        "F+ c #5953FF",
        "G+ c #E7ECF0",
        "H+ c #F1F4F7",
        "I+ c #7DAAFD",
        "J+ c #E3EDFA",
        "K+ c #C6DBF1",
        "L+ c #B7D1EE",
        "M+ c #A8C5EB",
        "N+ c #99BAE9",
        "O+ c #8AAEE7",
        "P+ c #7AA1E5",
        "Q+ c #6C95E4",
        "R+ c #5D88E3",
        "S+ c #4A77E3",
        "T+ c #3D6BE3",
        "U+ c #2F5EDE",
        "V+ c #2857DD",
        "W+ c #2C5AE5",
        "X+ c #2E59EB",
        "Y+ c #8298F6",
        "Z+ c #9497FF",
        "`+ c #4842FF",
        " @ c #E3EAF0",
        ".@ c #FDFDFE",
        "+@ c #C9DCFF",
        "@@ c #C4D8FD",
        "#@ c #C6DAF2",
        "$@ c #B2CCEE",
        "%@ c #A3C0EB",
        "&@ c #94B5E9",
        "*@ c #84A8E7",
        "=@ c #749BE6",
        "-@ c #658EE5",
        ";@ c #547FE4",
        ">@ c #4370E4",
        ",@ c #3B67E5",
        "'@ c #2755DD",
        ")@ c #2855E0",
        "!@ c #2D58EA",
        "~@ c #2D55ED",
        "{@ c #7A91F6",
        "]@ c #FEFEFF",
        "^@ c #AEB1FE",
        "/@ c #3834FF",
        "(@ c #E2E9EF",
        "_@ c #CEDDEB",
        ":@ c #EBF0F4",
        "<@ c #FCFCFD",
        "[@ c #87ADFD",
        "}@ c #D3E1FA",
        "|@ c #ADC7ED",
        "1@ c #9EBCEB",
        "2@ c #8EAFE9",
        "3@ c #7EA2E8",
        "4@ c #6E94E7",
        "5@ c #5F87E6",
        "6@ c #4E78E5",
        "7@ c #3D67E5",
        "8@ c #345EE3",
        "9@ c #2753DF",
        "0@ c #2954E6",
        "a@ c #2C55ED",
        "b@ c #728AF5",
        "c@ c #FCFDFF",
        "d@ c #C4C8FE",
        "e@ c #312EFF",
        "f@ c #DADADA",
        "g@ c #85AFD8",
        "h@ c #A9C8DC",
        "i@ c #D0DEE4",
        "j@ c #D7E2E8",
        "k@ c #E7EDF2",
        "l@ c #FBFCFD",
        "m@ c #F4F7FF",
        "n@ c #8EB0FE",
        "o@ c #B6CCF3",
        "p@ c #98B7EB",
        "q@ c #88AAEA",
        "r@ c #789BE8",
        "s@ c #688EE7",
        "t@ c #5880E7",
        "u@ c #4770E7",
        "v@ c #3A63E7",
        "w@ c #2953E1",
        "x@ c #2751E3",
        "y@ c #2B53EB",
        "z@ c #6A84F4",
        "A@ c #FBFBFF",
        "B@ c #D6D9FF",
        "C@ c #3433FF",
        "D@ c #6EABEF",
        "E@ c #BBDCE3",
        "F@ c #BCD8D8",
        "G@ c #BFD7DA",
        "H@ c #CBDBE2",
        "I@ c #D1DDE5",
        "J@ c #AEC5F4",
        "K@ c #B0C6FC",
        "L@ c #92B1EB",
        "M@ c #82A3EA",
        "N@ c #7395E9",
        "O@ c #6287E9",
        "P@ c #5177E8",
        "Q@ c #4067E9",
        "R@ c #355CE9",
        "S@ c #264EE2",
        "T@ c #274EE8",
        "U@ c #637FF5",
        "V@ c #F8F9FF",
        "W@ c #E6E8FF",
        "X@ c #3C3DFF",
        "Y@ c #83C0FF",
        "Z@ c #DCFFFF",
        "`@ c #DBFDFD",
        " # c #D1F1F1",
        ".# c #C4E4E4",
        "+# c #BBD8D8",
        "@# c #BDD6D9",
        "## c #6F96F4",
        "$# c #A3BAF5",
        "%# c #7799E8",
        "&# c #6B8EEA",
        "*# c #5C80EA",
        "=# c #4B6FEA",
        "-# c #3A60EA",
        ";# c #2B51E6",
        "># c #254CE6",
        ",# c #5A76F1",
        "'# c #F6F7FE",
        ")# c #F1F3FF",
        "!# c #4A4DFF",
        "~# c #7D7BFF",
        "{# c #5FABFF",
        "]# c #DAFDFF",
        "^# c #DDFFFF",
        "/# c #DAFDFD",
        "(# c #D0F1F1",
        "_# c #B9DBE6",
        ":# c #7A9CFC",
        "<# c #7697EA",
        "[# c #5B80E5",
        "}# c #4B70E5",
        "|# c #3E62E7",
        "1# c #3358EB",
        "2# c #274BE7",
        "3# c #506DED",
        "4# c #F2F4FE",
        "5# c #F9FAFF",
        "6# c #5B60FF",
        "7# c #7270FF",
        "8# c #76B1FF",
        "9# c #A5D5FF",
        "0# c #DCFEFF",
        "a# c #DBFFFF",
        "b# c #A1CAFE",
        "c# c #86A2FA",
        "d# c #557CE8",
        "e# c #4269E5",
        "f# c #3058E5",
        "g# c #4A67ED",
        "h# c #EFF1FE",
        "i# c #7177FF",
        "j# c #6566FF",
        "k# c #8EB9FF",
        "l# c #74A7FF",
        "m# c #ACD5FF",
        "n# c #DAFFFF",
        "o# c #769FFE",
        "p# c #829DF9",
        "q# c #4268EE",
        "r# c #2D55EC",
        "s# c #4666EE",
        "t# c #EBEEFD",
        "u# c #888FFF",
        "v# c #5256FF",
        "w# c #8BB0FF",
        "x# c #729DFF",
        "y# c #B1D8FF",
        "z# c #D6FCFF",
        "A# c #6B8CFE",
        "B# c #6582F7",
        "C# c #4566F3",
        "D# c #E7EBFE",
        "E# c #A2A8FF",
        "F# c #4248FF",
        "G# c #86A5FF",
        "H# c #7395FF",
        "I# c #B7DBFF",
        "J# c #D9FFFF",
        "K# c #C9F0FF",
        "L# c #758FFE",
        "M# c #E2E7FE",
        "N# c #B9BFFF",
        "O# c #3740FF",
        "P# c #8198FF",
        "Q# c #748EFF",
        "R# c #BBDFFF",
        "S# c #D8FFFF",
        "T# c #B3DAFF",
        "U# c #9BABFF",
        "V# c #CDD1FF",
        "W# c #3541FF",
        "X# c #7E8DFF",
        "Y# c #7589FF",
        "Z# c #C1E4FF",
        "`# c #8FB3FF",
        " $ c #B1BBFF",
        ".$ c #DEE1FF",
        "+$ c #3B49FF",
        "@$ c #7880FF",
        "#$ c #7785FF",
        "$$ c #617EFF",
        "%$ c #AFB8FF",
        "&$ c #4554FF",
        "*$ c #7272FF",
        "=$ c #3644FF",
        "-$ c #7A7FFF",
        "                                      . +                       ",
        "                                    @ # $ %                     ",
        "                                  & * = - ; >                   ",
        "                , ' '             ) ! ~ { ] ^                   ",
        "              / ( _ : < [ } |   1 2 3 4 5 6 7 8                 ",
        "              9 0 _ _ _ _ a b c d e f g h i j k                 ",
        "            l m _ _ _ _ _ n o p q r s t u v w x y               ",
        "            z _ _ _ _ _ A B C D E F G H I J K L M N             ",
        "          O P _ _ _ _ Q R S T U V W X Y Z `  ...+.@.            ",
        "          #._ _ _ _ _ $.%.&.*.=.-.;.>.,.'. .).!.~.{.].          ",
        "        ^./._ _ _ _ _ (._.:.<.[.}.|.1.2. .3.4.5.6.7.8.9.        ",
        "        0._ _ _ _ _ _ a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.      ",
        "      s.t._ _ _ _ _ _ u.v.w.x.y.z.A.B.C.k.D.E.F.G.H.I.J.K.L.    ",
        "      M._ _ _ _ _ _ N.O.P.Q.R.S.T.U.V.k.W.X.Y.Z.`. +.+++@+#+$+  ",
        "    %+a _ _ _ _ _ &+*+=+-+;+>+,+'+)+!+W.~+{+]+`.^+/+(+_+_ :+<+[+",
        "    }+_ _ _ _ _ |+1+2+3+4+5+6+7+8+9+0+a+b+c+d+e+f+g+h+_ _ _ i+j+",
        "  k+l+_ _ _ _ m+n+o+p+q+r+s+t+u+v+w+x+y+z+A+B+C+g+D+_ _ _ _ E+F+",
        "G+H+_ _ _ _ _ I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+g+Y+_ _ _ _ Z+`+  ",
        " @.@_ _ _ _ _ +@@@#@$@%@&@*@=@-@;@>@,@'@)@!@~@{@]@_ _ _ ^@/@    ",
        "  (@_@:@<@_ _ _ [@}@|@1@2@3@4@5@6@7@8@9@0@a@b@c@_ _ _ d@e@      ",
        "  f@g@h@i@j@k@l@m@n@o@p@q@r@s@t@u@v@w@x@y@z@A@_ _ _ B@C@        ",
        "      D@E@F@G@H@I@J@K@L@M@N@O@P@Q@R@S@T@U@V@_ _ _ W@X@          ",
        "      Y@Z@`@ #.#+#@###$#%#&#*#=#-#;#>#,#'#_ _ _ )#!#~#          ",
        "    {#]#^#Z@^#Z@/#(#_#:#<#[#}#|#1#2#3#4#_ _ _ 5#6#7#            ",
        "      8#9#0#Z@Z@a#a#a#b#c#d#e#f#2#g#h#_ _ _ ]@i#j#              ",
        "        k#l#m#a#a#n#n#n#o#p#q#r#s#t#_ _ _ _ u#v#                ",
        "            w#x#y#a#n#n#z#A#B#C#D#_ _ _ _ E#F#                  ",
        "                G#H#I#J#J#K#L#M#_ _ _ _ N#O#                    ",
        "                    P#Q#R#S#T#U#_ _ _ V#W#                      ",
        "                        X#Y#Z#`# $_ .$+$                        ",
        "                            @$#$$$%$&$                          ",
        "                                *$=$-$                          "
    ]))


class MyTreeView(QTreeView):

    def __init__(self):
        super(MyTreeView, self).__init__()

        self.createModel()
        self.createContextMenu()

        for idx in range(1, 4):
            self.hideColumn(idx)
        self.setAnimated(True)

    def createModel(self):
        model = QFileSystemModel()
        self.model = model
        self.setModel(model)
        self.setPath(getRootPath())

    def setPath(self, path):
        path = expandPath(path)

        if not isDir(path):
            return

        self.current_path = path
        self.model.setRootPath(path)
        self.setRootIndex(self.model.index(path))

    def filterFiles(self, regex):
        if regex == "" or regex == "*":
            regex = ".*"

        for idx in range(self.countItems()):
            if not re.search(regex,
                             self.getFileName(idx),
                             flags=re.IGNORECASE):
                self.setRowHidden(idx, self.getCurrentModelIndex(), True)
            else:
                self.setRowHidden(idx, self.getCurrentModelIndex(), False)

    def getCurrentModelIndex(self):
        return self.model.index(self.current_path)

    def getFileName(self, number):
        return self.model.fileName(self.getModelIndexOfItem(number))

    def getModelIndexOfItem(self, number):
        parent = self.getCurrentModelIndex()
        index = self.model.index(number, 0, parent)
        return index

    def countItems(self):
        index = self.getCurrentModelIndex()
        return self.model.rowCount(index)

    def selectedItems(self):
        list = self.selectedIndexes()
        res = []
        for modelIndex in list:
            res.append(self.model.filePath(modelIndex))
        return res

    def selectionChanged(self, selected, deselected):
        print("\tSelection:")
        items = self.selectedItems()
        for item in items:
            print(item)
        print("")

    def mouseDoubleClickEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton \
           and len(self.selectedItems()) != 0:
            item = self.selectedItems()[0]
            os.system('emacsclient --eval \'(find-file "{0}")\''.format(item))

    def copyFullPath(self):
        items = self.selectedItems()
        if len(items) == 1:
            QApplication.clipboard().setText(items[0])

    def copyFileName(self):
        items = self.selectedItems()
        if len(items) == 1:
            QApplication.clipboard().setText(getFileName(items[0]))

    def deleteFile(self):
        items = self.selectedItems()
        if len(items) == 1:
            path = items[0]
            if showDeleteFileConfirmDialog(path):
                self.model.remove(self.model.index(path))

    def gotoDir(self):
        items = self.selectedItems()
        if len(items) == 1:
            path = items[0]
            setPath(path)

    def createContextMenu(self):
        menu = QMenu(self)

        gotoDirAction = QAction("&Go to dir", self)
        gotoDirAction.triggered.connect(self.gotoDir)

        copyFullPathAction = QAction("&Copy full path", self)
        copyFullPathAction.triggered.connect(self.copyFullPath)

        copyFileNameAction = QAction("Copy &file name", self)
        copyFileNameAction.triggered.connect(self.copyFileName)

        deleteFileAction = QAction("&Delete file", self)
        deleteFileAction.triggered.connect(self.deleteFile)

        menu.addAction(gotoDirAction)
        menu.addAction(copyFullPathAction)
        menu.addAction(copyFileNameAction)
        menu.addAction(deleteFileAction)

        self.menu = menu

    def showContextMenu(self, event):
        self.menu.exec_(event.globalPos())

    def contextMenuEvent(self, event):
        """Activated when right-clicking."""

        if len(self.selectedItems()) != 0:
            self.showContextMenu(event)


class MainWindow(QWidget):

    def __init__(self):
        super(MainWindow, self).__init__()

        self.resize(350, 650)
        self.move(0, 0)
        self.setWindowTitle("File Browser for Emacs Server")

        self.createChildren()
        self.setWindowIcon(getOpenDirIcon())
        self.show()

    def createChildren(self):
        box = QVBoxLayout()
        self.createPathEntry()
        self.createFilterEntry()
        self.createTreeView()

        self.pathEntry.textChanged.connect(self.tree.setPath)
        self.pathEntry.textEdited.connect(self.clearFilterEntry)
        self.filterEntry.textEdited.connect(self.tree.filterFiles)

        box.addLayout(self.pathEntryLayout)
        box.addWidget(self.filterEntry)
        box.addWidget(self.tree)
        box.setContentsMargins(0, 0, 0, 0)

        self.setLayout(box)
        self.box = box

    def createFilterEntry(self):
        fil = QLineEdit()
        fil.setText("*")

        focusShortcut = QShortcut(QKeySequence("Ctrl+F"), fil)
        focusShortcut.activated.connect(fil.setFocus)

        self.filterEntry = fil

    def createPathEntry(self):
        box = QHBoxLayout()

        button = QToolButton()
        action = QAction(getOpenDirIcon(), "Browse (Ctrl+O)", None)
        action.setShortcut(QKeySequence("Ctrl+O"))
        action.triggered.connect(self.browseDir)
        button.setDefaultAction(action)

        pathEntry = QLineEdit()
        pathEntry.setText(getRootPath())
        focusShortcut = QShortcut(QKeySequence("Ctrl+L"), pathEntry)
        focusShortcut.activated.connect(pathEntry.setFocus)

        box.addWidget(button)
        box.addWidget(pathEntry)

        self.pathEntry = pathEntry
        self.pathEntryLayout = box
        self.browseDirButton = button

    def createTreeView(self):
        tree = MyTreeView()

        self.tree = tree

    def clearFilterEntry(self, path):
        self.filterEntry.setText("*")

    def getCurrentPath(self):
        return expandPath(self.pathEntry.text())

    def browseDir(self):
        """Open a dialog to browse to a directory"""
        print(self.getCurrentPath())
        path = QFileDialog.getExistingDirectory(None,
                                                "Open Directory",
                                                self.getCurrentPath())
        if path != "":
            self.setPath(path)

    def setPath(self, path):
        self.pathEntry.setText(path)

    def getPath(self):
        return self.pathEntry.text()

    def closeEvent(self, event):
        """Before closing, save the current path into the config file"""
        with open(expandPath(CONFIG_FILE), "w") as f:
            f.write(self.getPath())
        event.accept()


def pathExists(path):
    return os.path.exists(path)


def expandPath(path):
    return os.path.abspath(os.path.expanduser(path))


def reverse(aStr):
    return aStr[::-1]


def getFileName(path):
    return reverse(re.split(r"/[^/]+", reverse(path), 1)[0])


def getRootPath():
    configFile = expandPath(CONFIG_FILE)

    if pathExists(configFile):
        rootPath = ""
        with open(configFile, "r") as f:
            rootPath = f.readline()

        if rootPath[-1] == "\n":
            rootPath = rootPath[:-1]

        return [expandPath(DEFAULT_ROOT_PATH), rootPath][pathExists(rootPath)]

    return expandPath(DEFAULT_ROOT_PATH)


def isDir(path):
    return os.path.isdir(expandPath(path))


def setPath(path):
    mainWin.setPath(path)


def showDeleteFileConfirmDialog(path):
    msgbox = QMessageBox()
    msgbox.setText("You are about to delete a file!")
    msgbox.setInformativeText("Are you sure you want to remove\n{0}\nfrom your system?".format(path))
    msgbox.setStandardButtons(QMessageBox.Yes | QMessageBox.No)
    msgbox.setDefaultButton(QMessageBox.No)
    msgbox.setIcon(QMessageBox.Critical)
    return [True, False][msgbox.exec_() == QMessageBox.No]


app = QApplication(sys.argv)
mainWin = MainWindow()
sys.exit(app.exec_())
