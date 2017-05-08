import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

ApplicationWindow {
  title: "Drag demo"
  width: 800
  height: 600
  visible: true

  Rectangle {
    id: container
    width: 800; height: 600

    SystemPalette { id: pal; colorGroup: SystemPalette.Active }

    Canvas {
      id: paths
      anchors.fill: parent
      contextType: "2d"

      Path {
          id: myPath
          startX: 0; startY: 100

          PathCurve { x: 75; y: 75 }
          PathCurve { x: 200; y: 150 }
          PathCurve { x: 325; y: 25 }
          PathCurve { x: rect.x; y: rect.y }
      }

      onPaint: {
          context.fillStyle = pal.window;
          context.fillRect(0, 0, width, height);
          context.strokeStyle = Qt.rgba(.4,.6,.8);
          context.path = myPath;
          context.stroke();
      }
    }

    Rectangle {
      id: rect
      width: 50; height: 50
      color: "red"

      onXChanged: paths.requestPaint()

      MouseArea {
        anchors.fill: parent
        drag.target: rect
      }

      Rectangle {
        id: conn
        y: 10
        width: 10; height: 5
        color: "yellow"

        MouseArea {
          anchors.fill: parent
          onClicked: console.log(rect.x, ", ", rect.y)
        }
      }
    }
  }
}