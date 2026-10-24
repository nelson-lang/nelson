%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef StackedLineProperties
  properties
    Color = []
    MarkerFaceColor = 'none'
    MarkerEdgeColor = []
    LineStyle = '-'
    LineWidth = 0.5
    Marker = 'none'
    MarkerSize = 6
    PlotType = 'plot'
  end

  methods
    function obj = StackedLineProperties(color, markerFaceColor, markerEdgeColor, lineStyle, ...
        lineWidth, marker, markerSize, plotType)
      if nargin == 0
        return
      end
      obj.Color = color;
      obj.MarkerFaceColor = markerFaceColor;
      obj.MarkerEdgeColor = markerEdgeColor;
      obj.LineStyle = lineStyle;
      obj.LineWidth = lineWidth;
      obj.Marker = marker;
      obj.MarkerSize = markerSize;
      obj.PlotType = plotType;
    end
  end
end
