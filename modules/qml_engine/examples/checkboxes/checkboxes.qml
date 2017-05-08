import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0


ApplicationWindow {
    title: "Checkbox example"
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
                CheckBox { id:chk5; text: "checkbox 5"; checked: true }
                CheckBox { id:chk6; text: "checkbox 6"; checked: false }
                CheckBox { id:chk7; text: "checkbox 7"; checked: true }
                CheckBox { id:chk8; text: "checkbox 8"; checked: false }
                CheckBox { id:chk9; text: "checkbox 9"; checked: true }
            }
        }

    }
    Button {
        id: btnValid
        x:510
        y:110
        text: "Valid"
        onClicked: {
            res.result = chk1.checked + 2*chk2.checked+4*chk3.checked + 8*chk4.checked
            res.result += 16*chk5.checked + 32*chk6.checked + 64*chk7.checked + 128*chk8.checked +256*chk9.checked

        }
    }
}