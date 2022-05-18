import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 2.12



Window {
    id: mainWindow
    visible: true
    width: 360
    height: 360
    title: "Qml Window"
    objectName: "basic_window"
    Text {
        id: text
        text: "Hello world!"
        objectName: "text1"
    }
   

    Button {
        text: "Button"
        anchors.centerIn: parent 
        onClicked: nelson.disp('button pressed.')
        objectName: "myButton"
    }
}