import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 2.12



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