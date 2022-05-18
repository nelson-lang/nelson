import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 2.12


Window {
    id: mainWindow
    visible: false
    width: 360
    height: 360
    title: "test_qml_methods"
    objectName: "MyWindow"
    Text {
        id: text
        text: "Hello world!"
        objectName: "text1"
    }

   Item {
    function myQmlFunction1(msg) {
        return [msg, 1,2,3]
        }
        
    function myQmlFunction2() {
        return [1,2,3]
        }

    function myQmlFunction3() {
        a = 1 + 1;
        }

    function myQmlFunction4() {
        nelson.call('rrrr');
        }

   }

}
