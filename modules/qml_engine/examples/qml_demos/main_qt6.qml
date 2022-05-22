
import QtQuick
import QtQuick.Controls

import "content"

ApplicationWindow {
    visible: true
    width: 500
    height: 700
    objectName: "qml_demos"

    Rectangle {
        color: "#212126"
        anchors.fill: parent
    }

    ListModel {
        id: pageModel
        ListElement {
            title: "qcharts <--> Nelson"
            page: "qcharts_demo"
        }
        ListElement {
            title: "clock"
            page: "clock"
        }
        ListElement {
            title: "drag demo"
            page: "drag_demo"
        }
        ListElement {
            title: "basic layouts"
            page: "basic_layouts"
        }
        ListElement {
            title: "basic window"
            page: "basic_window"
        }
        ListElement {
            title: "checkboxes"
            page: "checkboxes"
        }
        ListElement {
            title: "colors"
            page: "colors"
        }
        ListElement {
            title: "dynamic scene"
            page: "dynamic_scene_demo"
        }
    }

    StackView {
        id: stackView
        anchors.fill: parent
        // Implements back key navigation
        focus: true
        Keys.onReleased: if (event.key === Qt.Key_Back && stackView.depth > 1) {
                             stackView.pop();
                             event.accepted = true;
                         }

        initialItem: Item {
            width: parent.width
            height: parent.height
            ListView {
                model: pageModel
                anchors.fill: parent
                delegate: AndroidDelegate {
                    text: title
                    onClicked: nelson.call('callback_demos_run', page)
                }
            }
        }
    }

}
