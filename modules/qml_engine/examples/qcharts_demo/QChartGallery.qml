/* QChartGallery.qml --- 
 * 
 * Author: Julien Wintz
 * Created: Thu Feb 13 23:41:59 2014 (+0100)
 * Version: 
 * Last-Updated: Fri Feb 14 12:58:42 2014 (+0100)
 *           By: Julien Wintz
 *     Update #: 5
 */

/* Change Log:
 * 
 */

import QtQuick 2.0

import "."
import "QChart.js"        as Charts
import "QChartGallery.js" as ChartsData

Rectangle {

  property int chart_width: 300;
  property int chart_height: 300;
  property int chart_spacing: 20;
  property int text_height: 80;
  property int row_height: 8;

  color: "#ffffff";
  width: chart_width*3 + 2*chart_spacing;
  height: chart_height*2 + chart_spacing + 2*row_height + text_height;

// /////////////////////////////////////////////////////////////////
// Header
// /////////////////////////////////////////////////////////////////

  Rectangle { color: "#282b36"; width: parent.width/1.0; height: row_height; }
  Rectangle { color: "#f33e6f"; width: parent.width/3.0; height: row_height; x: 0*width; y: height; }
  Rectangle { color: "#46bfbd"; width: parent.width/3.0; height: row_height; x: 1*width; y: height; }
  Rectangle { color: "#fbd45c"; width: parent.width/3.0; height: row_height; x: 2*width; y: height; }

  Text {

    y: 2*row_height;

    width: parent.width;
    height: text_height;

    text: "QChart.js";
    font.pointSize: 32;

    horizontalAlignment: Text.AlignHCenter;
    verticalAlignment: Text.AlignVCenter;

    Rectangle {

      id: button;

      anchors.top:  parent.top;
      anchors.topMargin: (parent.height-parent.font.pointSize)/2;
      anchors.right: parent.right;
      anchors.rightMargin: (parent.height-parent.font.pointSize)/2;

      width: 100;
      height: 32;

      color: "#2d91ea";
      radius: 8;

      Text {
        anchors.centerIn: parent;
        color: "#ffffff";
        text: "Repaint";
        font.bold: true;
      }

      MouseArea {
        anchors.fill: parent;
        onPressed: {
          button.color = "#1785e6"
        }
        onReleased: {
          button.color = "#2d91ea"
          chart_bar.repaint();
          chart_doughnut.repaint();
          chart_line.repaint();
          chart_pie.repaint();
          chart_polar.repaint();
          chart_radar.repaint();
        }
      }
    }
  }

// /////////////////////////////////////////////////////////////////
// Body
// /////////////////////////////////////////////////////////////////

  Grid {

    id: layout;

    x: 0;
    y: 2*row_height + text_height;

    width: parent.width;
    height: parent.height - 2*row_height - text_height;

    columns: 3;
    spacing: chart_spacing;

    Chart {
      id: chart_line;
      width: chart_width;
      height: chart_height;
      chartAnimated: true;
      chartAnimationEasing: Easing.InOutElastic;
      chartAnimationDuration: 2000;
      chartData: ChartsData.ChartLineData;
      chartType: Charts.ChartType.LINE;
    }

    Chart {
      id: chart_polar;
      width: chart_width;
      height: chart_height;
      chartAnimated: true;
      chartAnimationEasing: Easing.InBounce;
      chartAnimationDuration: 2000;
      chartData: ChartsData.ChartPolarData;
      chartType: Charts.ChartType.POLAR;
    }

    Chart {
      id: chart_radar;
      width: chart_width;
      height: chart_height;
      chartAnimated: true;
      chartAnimationEasing: Easing.OutBounce;
      chartAnimationDuration: 2000;
      chartData: ChartsData.ChartRadarData;
      chartType: Charts.ChartType.RADAR;
    }

    Chart {
      id: chart_pie;
      width: chart_width;
      height: chart_height;
      chartAnimated: true;
      chartAnimationEasing: Easing.Linear;
      chartAnimationDuration: 2000;
      chartData: ChartsData.ChartPieData;
      chartType: Charts.ChartType.PIE;
    }

    Chart {
      id: chart_bar;
      width: chart_width;
      height: chart_height;
      chartAnimated: true;
      chartAnimationEasing: Easing.OutBounce;
      chartAnimationDuration: 2000;
      chartData: ChartsData.ChartBarData;
      chartType: Charts.ChartType.BAR;
    }

    Chart {
      id: chart_doughnut;
      width: chart_width;
      height: chart_height;
      chartAnimated: true;
      chartAnimationEasing: Easing.OutElastic;
      chartAnimationDuration: 2000;
      chartData: ChartsData.ChartDoughnutData;
      chartType: Charts.ChartType.DOUGHNUT;
    }
  }
}
