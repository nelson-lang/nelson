/* QChart.qml ---
 *
 * Author: Julien Wintz
 * Created: Thu Feb 13 20:59:40 2014 (+0100)
 * Version:
 * Last-Updated: jeu. mars  6 12:55:14 2014 (+0100)
 *           By: Julien Wintz
 *     Update #: 69
 */

/* Change Log:
 *
 */

import QtQuick 2.0

import "QChart.js" as Charts

Canvas {

  id: canvas;

// ///////////////////////////////////////////////////////////////

  property   var chart;
  property   var chartData;
  property   int chartType: 0;
  property  bool chartAnimated: true;
  property alias chartAnimationEasing: chartAnimator.easing.type;
  property alias chartAnimationDuration: chartAnimator.duration;
  property   int chartAnimationProgress: 0;
  property   var chartOptions: ({})

// /////////////////////////////////////////////////////////////////
// Callbacks
// /////////////////////////////////////////////////////////////////

  onPaint: {
      var ctx = canvas.getContext("2d");
      /* Reset the canvas context to allow resize events to properly redraw
         the surface with an updated window size */
      ctx.reset()

      switch(chartType) {
      case Charts.ChartType.BAR:
          chart = new Charts.Chart(canvas, ctx).Bar(chartData, chartOptions);
          break;
      case Charts.ChartType.DOUGHNUT:
          chart = new Charts.Chart(canvas, ctx).Doughnut(chartData, chartOptions);
          break;
      case Charts.ChartType.LINE:
          chart = new Charts.Chart(canvas, ctx).Line(chartData, chartOptions);
          break;
      case Charts.ChartType.PIE:
          chart = new Charts.Chart(canvas, ctx).Pie(chartData, chartOptions);
          break;
      case Charts.ChartType.POLAR:
          chart = new Charts.Chart(canvas, ctx).PolarArea(chartData, chartOptions);
          break;
      case Charts.ChartType.RADAR:
          chart = new Charts.Chart(canvas, ctx).Radar(chartData, chartOptions);
          break;
      default:
          console.log('Chart type should be specified.');
      }

      chart.init();

      if (chartAnimated)
          chartAnimator.start();
      else
          chartAnimationProgress = 100;

      chart.draw(chartAnimationProgress/100);
  }

  onHeightChanged: {
    requestPaint();
  }

  onWidthChanged: {
    requestPaint();
  }

  onChartAnimationProgressChanged: {
      requestPaint();
  }

  onChartDataChanged: {
      requestPaint();
  }

// /////////////////////////////////////////////////////////////////
// Functions
// /////////////////////////////////////////////////////////////////

  function repaint() {
      chartAnimationProgress = 0;
      chartAnimator.start();
  }

// /////////////////////////////////////////////////////////////////
// Internals
// /////////////////////////////////////////////////////////////////

  PropertyAnimation {
             id: chartAnimator;
         target: canvas;
       property: "chartAnimationProgress";
             to: 100;
       duration: 500;
    easing.type: Easing.InOutElastic;
  }
}
