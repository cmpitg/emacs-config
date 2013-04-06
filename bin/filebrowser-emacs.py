#!/usr/bin/env python3

#
# TODO:
#
# * Passing directory path
# * File filter
#


import sys
import os
from PySide import QtCore, QtGui


ROOT_PATH = "/home/cmpitg/Desktop/"


class MyTreeView(QtGui.QTreeView):

    def __init__(self):
        super(MyTreeView, self).__init__()

        self.createModel()

        for idx in range(1, 4):
            self.hideColumn(idx)
        self.setAnimated(True)
        self.setWindowTitle("Directory Tree View")
        # self.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)

    def createModel(self):
        model = QtGui.QFileSystemModel()
        self.model = model
        self.setModel(model)
        self.setPath(ROOT_PATH)

    def setPath(self, path):
        path = os.path.abspath(path)

        if not os.path.isdir(path):
            return

        self.model.setRootPath(path)
        self.setRootIndex(self.model.index(path))

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

        self.resize(800, 600)
        self.createChildren()
        self.show()

    def createChildren(self):
        box = QtGui.QVBoxLayout()
        self.createPathEntry()
        self.createTreeView()

        self.pathEntry.textChanged.connect(self.tree.setPath)

        box.addWidget(self.pathEntry)
        box.addWidget(self.tree)
        box.setContentsMargins(0, 0, 0, 0)

        self.setLayout(box)
        self.box = box

    def createPathEntry(self):
        pathEntry = QtGui.QLineEdit()
        pathEntry.setText(ROOT_PATH)

        self.pathEntry = pathEntry

    def createTreeView(self):
        tree = MyTreeView()

        self.tree = tree
        

app = QtGui.QApplication(sys.argv)
mainWin = MainWindow()
sys.exit(app.exec_())
