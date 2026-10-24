%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef StackedAxesProperties
  properties
    YLimits = []
    YScale = 'linear'
    LegendLabels = {}
    LegendLocation = 'northeast'
    LegendVisible = 'off'
    CollapseLegend = 'off'
  end

  methods
    function obj = StackedAxesProperties(yLimits, yScale, legendLabels, legendLocation, ...
        legendVisible, collapseLegend)
      if nargin == 0
        return
      end
      obj.YLimits = yLimits;
      obj.YScale = yScale;
      obj.LegendLabels = legendLabels;
      obj.LegendLocation = legendLocation;
      obj.LegendVisible = legendVisible;
      obj.CollapseLegend = collapseLegend;
    end
  end
end
