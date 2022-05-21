import QtQuick
import QtQuick.Window
import QtQuick.Controls



Rectangle {
    id: mainWindow
    visible: false
    width: 360
    height: 360
    objectName: "MyWindow"
    Text {
        id: text
        text: "Hello world!"
        objectName: "text1"
    }

}