
import QtQuick 2.2
import QtQuick.Controls 1.1
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

    toolBar: BorderImage {
        border.bottom: 8
        source: "images/toolbar.png"
        width: parent.width
        height: 100

        Rectangle {
            id: backButton
            width: opacity ? 60 : 0
            anchors.left: parent.left
            anchors.leftMargin: 20
            opacity: stackView.depth > 1 ? 1 : 0
            anchors.verticalCenter: parent.verticalCenter
            antialiasing: true
            height: 60
            radius: 4
            color: backmouse.pressed ? "#222" : "transparent"
            Behavior on opacity { NumberAnimation{} }
            Image {
                anchors.verticalCenter: parent.verticalCenter
                source: "images/navigation_previous_item.png"
            }
            MouseArea {
                id: backmouse
                anchors.fill: parent
                anchors.margins: -10
                onClicked: stackView.pop()
            }
        }

        Text {
            font.pixelSize: 42
            Behavior on x { NumberAnimation{ easing.type: Easing.OutCubic} }
            x: backButton.x + backButton.width + 20
            anchors.verticalCenter: parent.verticalCenter
            color: "white"
            text: "Nelson QML demos"
        }
    }

    ListModel {
        id: pageModel
        ListElement {
            title: "d3 demo"
            page: "d3_demo"
        }
        ListElement {
            title: "qcharts <--> Nelson"
            page: "qcharts_demo"
        }
        ListElement {
            title: "threejs <--> Nelson"
            page: "threejs_demo"
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
            title: "component gallery"
            page: "component_gallery"
        }
        ListElement {
            title: "dynamic scene"
            page: "dynamic_scene_demo"
        }
        ListElement {
            title: "widget gallery"
            page: "widget_gallery"
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
