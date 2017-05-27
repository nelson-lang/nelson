import QtQuick 2.2
import "."
import QtSensors 5.0
import "d3.min.js" as D3

Rectangle {
    id: bubbleContainer
    width: 800
    height: 600
    clip: true

    property int numbubbles: 10;
    property real bubbleScaleFactor: 1
    property real maxBubbleRadius: width*0.05*bubbleScaleFactor + width/10*bubbleScaleFactor
    property variant bubblesprites: []

    function createBubbles() {
        var bubblecomponent = Qt.createComponent("bubble.qml");
        var radius = Math.random()*width*0.15*bubbleScaleFactor + width/20*bubbleScaleFactor
        bubblesprites.push(bubblecomponent.createObject(bubbleContainer, {"x": width/2-radius , "y": height/2 - radius, "radius":radius, "color":  "red" } ));
        var xLeft = -radius
        var xRight = radius
        for (var i=1; i < numbubbles; ++i) {
            var radius2 = Math.random()*width*0.15*bubbleScaleFactor + width/20*bubbleScaleFactor
            var y = height*(0.5 + (Math.random()-0.5))-radius2
            if (i % 2 === 0) {
                bubblesprites.push(bubblecomponent.createObject(bubbleContainer, {"x": xLeft, "y": y, "radius":radius2 } ));
                xLeft += radius2*2
            } else {
                xRight -= radius2*2
                bubblesprites.push(bubblecomponent.createObject(bubbleContainer, {"x": xRight, "y": y, "radius":radius2 } ));
            }
        }
    }

    function boundParticle(b) {
        if (b.y > height - b.radius)
            b.y = height - b.radius
        if (b.y < b.radius)
            b.y = b.radius
        if (b.x > width*2.5)
            b.x = width*2.5
        if (b.x < -width*2)
            b.x = -width*2
    }

    property real padding: width/100*bubbleScaleFactor;
    function collide(node) {
        var r = node.radius + maxBubbleRadius+10,
              nx1 = node.x - r,
              nx2 = node.x + r,
              ny1 = node.y - r,
              ny2 = node.y + r;
        boundParticle(node)
        return function(quad, x1, y1, x2, y2) {
            if (quad.point && (quad.point !== node)) {
              var x = node.x - quad.point.x,
                  y = node.y - quad.point.y,
                  l = Math.sqrt(x * x + y * y),
                  r = node.radius + quad.point.radius + padding;
              if (l < r) {
                l = (l - r) / l * .5;
                node.x -= x *= l;
                node.y -= y *= l;
                quad.point.x += x;
                quad.point.y += y;
              }
            }
            return x1 > nx2 || x2 < nx1 || y1 > ny2 || y2 < ny1;
        };
    }

    property var nodes;
    property var force;

    Component.onCompleted: {
        initializeTimer.start()
    }

    Timer {
        id: initializeTimer
        interval: 20;
        running: false;
        repeat: false;
        onTriggered: {
            createBubbles();
            nodes = D3.d3.range(numbubbles).map(function() { return {radius: bubblesprites[this.index]}; });
            for(var i = 0; i < numbubbles; ++i) {
                nodes[i].radius = bubblesprites[i].radius;
                nodes[i].px = nodes[i].x = bubblesprites[i].x+bubblesprites[i].radius
                nodes[i].py = nodes[i].y = bubblesprites[i].y+bubblesprites[i].radius
            }
            nodes[0].fixed = true;

            force = D3.d3.layout.force().gravity(0.05).charge(function(d, i) { return 0; }).nodes(nodes).size([width,height])
            force.start()
            nodes[0].px = width/2
            nodes[0].py = height/2
            force.on("tick", function(e) {
                var q = D3.d3.geom.quadtree(nodes),i = 0,
                        n = nodes.length;
                while (++i < n) q.visit(collide(nodes[i]));
                for(var i = 0; i < numbubbles; ++i) {
                    bubblesprites[i].x = nodes[i].x - nodes[i].radius;
                    bubblesprites[i].y = nodes[i].y - nodes[i].radius;
                }
            });
            timer.start();
        }
    }

    property real tilt: 0
    function applyTilt() {
        if (Math.abs(tilt) >= 10) {
            for(var i = 1;  i < bubblesprites.length ; ++i) {
                nodes[i].px += tilt/4
            }
        }
    }

    focus: true
    Keys.onPressed: {
        if (event.key === Qt.Key_Left)
            tilt = -30
        else if(event.key === Qt.Key_Right)
            tilt = 30
        tiltSensor.active = false
    }

    Keys.onReleased: {
        tilt = 0
        tiltSensor.active = true
    }

    TiltSensor {
        id: tiltSensor
        active: false
        onReadingChanged: {
            tilt = tiltSensor.reading.yRotation;
        }
    }

    Timer {
        id: timer
        interval: 26;
        running: false;
        repeat: true;
        onTriggered: {
            applyTilt();
            force.resume();
            force.tick();
        }
    }
}
