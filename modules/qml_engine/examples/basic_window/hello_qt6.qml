import QtQuick
import QtQuick.Window
import QtQuick.Controls



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