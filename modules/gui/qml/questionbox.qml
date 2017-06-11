import QtQuick 2.3
import QtQuick.Window 2.0
import QtQuick.Controls 1.2

    Window {
        property string wasClicked : "";
        id: dialog
        width: 405
        height: 190
        visible: false

        Rectangle {
            width: dialog.width
            height: dialog.height
            Image {
                id: image1
                objectName: "iconImage"
                anchors.top: parent.top
                anchors.topMargin: 1
                fillMode: Image.Stretch
                anchors.left: parent.left
                source: "question_icon.png"
            }

            TextArea {
                id: textArea1
                objectName: "textArea"
                width: 265
                height: 156
                text: ""
                readOnly: true
                font.pointSize: 9
                wrapMode: Text.WrapAnywhere
                anchors.top: parent.top
                anchors.topMargin: 1
                anchors.left: image1.right
                anchors.right: parent.right
                anchors.rightMargin: 1
            }


            Row {
                anchors.top: textArea1.bottom
                anchors.bottom: parent.bottom
                anchors.right: parent.right

                Button {
                    id: buttonLeft
                    objectName: "buttonLeft"
                    text: "Yes"
                    isDefault: false
                    focus: false
                    onClicked: {
                        wasClicked = objectName
                        dialog.visible = false
                    }
                    Keys.onPressed: {
                        if (event.key == Qt.Key_Return || event.key == Qt.Key_Enter) {
                            wasClicked = objectName
                            dialog.visible = false
                        } else if (event.key == Qt.Key_Escape) {
                            wasClicked = ""
                            dialog.visible = false
                        }
                    }
                }

                Button {
                    id: buttonMiddle
                    objectName: "buttonMiddle"
                    text: "No"
                    isDefault: false
                    focus: false
                    onClicked: {
                        wasClicked = objectName
                        dialog.visible = false
                    }
                    Keys.onPressed: {
                        if (event.key == Qt.Key_Return || event.key == Qt.Key_Enter) {
                            wasClicked = objectName
                            dialog.visible = false
                        } else if (event.key == Qt.Key_Escape) {
                            wasClicked = ""
                            dialog.visible = false
                        }
                    }
                }

                Button {
                    id: buttonRight
                    objectName: "buttonRight"
                    text: "Cancel"
                    isDefault: false
                    focus: false
                    onClicked: {
                        wasClicked = objectName
                        dialog.visible = false
                    }
                    Keys.onPressed: {
                        if (event.key == Qt.Key_Return || event.key == Qt.Key_Enter) {
                            wasClicked = objectName
                            dialog.visible = false
                        } else if (event.key == Qt.Key_Escape) {
                            wasClicked = ""
                            dialog.visible = false
                        }
                    }
                }
            }
        }
    }
