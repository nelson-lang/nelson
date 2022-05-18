import QtQuick 2.0
import QtQuick.Controls 2.12
import QtQuick.Layouts 1.0


ApplicationWindow {
    title: "Checkbox example"
    objectName: "checkboxes"

    width: 620
    height: 420
    visible: true

    Item {
        id: res
        property double result: 0.0
    }

    ColumnLayout{
        id: page
        height: parent.height
        width: 500
        x:30
        Text {
            id: helloText
            text: "Checkbox selector"
            anchors.top: parent.top
            anchors.topMargin: 10
            anchors.horizontalCenter: page.horizontalCenter
            font.pointSize: 24; font.bold: true
        }
        GroupBox {
            title: "Check some checkboxes:   "
            anchors.left: parent.left
            anchors.top: parent.top
            anchors.leftMargin: 20
            anchors.topMargin: 60
            ColumnLayout {
                y: 20
                CheckBox { id:chk1; text: "checkbox 1"; checked: true }
                CheckBox { id:chk2; text: "checkbox 2"; checked: false }
                CheckBox { id:chk3; text: "checkbox 3"; checked: true }
                CheckBox { id:chk4; text: "checkbox 4"; checked: false }
            }
        }

    }
    Button {
        id: btnValid
        x:510
        y:110
        text: "Valid"
        onClicked: {
          nelson.call('callback_checkboxes', chk1.checked, chk2.checked, chk3.checked, chk4.checked);
        }
    }
}