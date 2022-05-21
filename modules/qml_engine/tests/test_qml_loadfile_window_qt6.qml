import QtQuick
import QtQuick.Window
import QtQuick.Controls



Window {
    id: mainWindow
    visible: false
    width: 360
    height: 360
    title: "test_qvariant"
    objectName: "MyWindow"
    Text {
        id: text
        text: "Hello world!"
        objectName: "text1"
    }

}