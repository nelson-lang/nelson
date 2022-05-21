import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.1



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