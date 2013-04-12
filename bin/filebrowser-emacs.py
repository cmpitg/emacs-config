#!/usr/bin/env python3

#
# TODO:
#
# * Passing directory path
#
# * Shortcut:
#   - <Ctrl L> to jump to path entry
#   - <Ctrl F> to jump to filter entry
#
# * Right-click context menu:
#   - Copy full path
#   - Copy file name
#   - Delete file
#
# * File filter (Done)
#
# * Expand path (Done)
#


import sys
import os
import re

from PySide import QtCore, QtGui


ROOT_PATH = "~/Desktop/"


class MyTreeView(QtGui.QTreeView):

    def __init__(self):
        super(MyTreeView, self).__init__()

        self.createModel()

        for idx in range(1, 4):
            self.hideColumn(idx)
        self.setAnimated(True)

    def createModel(self):
        model = QtGui.QFileSystemModel()
        self.model = model
        self.setModel(model)
        self.setPath(ROOT_PATH)

    def setPath(self, path):
        path = expandPath(path)

        if not os.path.isdir(path):
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
        if event.button() == QtCore.Qt.MouseButton.LeftButton \
           and len(self.selectedItems()) != 0:
            item = self.selectedItems()[0]
            os.system('emacsclient --eval \'(find-file "{0}")\''.format(item))


class MainWindow(QtGui.QWidget):

    def __init__(self):
        super(MainWindow, self).__init__()

        self.resize(350, 650)
        self.move(0, 0)
        self.setWindowTitle("File Browser for Emacs Server")

        self.createChildren()
        self.show()

    def createChildren(self):
        box = QtGui.QVBoxLayout()
        self.createPathEntry()
        self.createFilterEntry()
        self.createTreeView()

        self.pathEntry.textChanged.connect(self.tree.setPath)
        self.pathEntry.textEdited.connect(self.clearFilterEntry)
        self.filterEntry.textEdited.connect(self.tree.filterFiles)

        box.addWidget(self.pathEntry)
        box.addWidget(self.filterEntry)
        box.addWidget(self.tree)
        box.setContentsMargins(0, 0, 0, 0)

        self.setLayout(box)
        self.box = box

    def createFilterEntry(self):
        fil = QtGui.QLineEdit()
        fil.setText("*")

        self.filterEntry = fil

    def createPathEntry(self):
        pathEntry = QtGui.QLineEdit()
        pathEntry.setText(ROOT_PATH)

        self.pathEntry = pathEntry

    def createTreeView(self):
        tree = MyTreeView()

        self.tree = tree

    def clearFilterEntry(self, path):
        self.filterEntry.setText("*")


def expandPath(path):
    return os.path.abspath(os.path.expanduser(path))
        

app = QtGui.QApplication(sys.argv)
mainWin = MainWindow()
sys.exit(app.exec_())
