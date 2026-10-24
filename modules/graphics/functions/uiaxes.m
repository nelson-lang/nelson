%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ax = uiaxes(varargin)
  % uiaxes: create axes for App Designer style apps (uifigure based).
  % Behaves like axes with UIAxes defaults:
  %   Units = 'pixels', Position = [10 10 400 300],
  %   NextPlot = 'replacechildren', FontUnits = 'pixels'
  parent = [];
  args = varargin;
  if ~isempty(args) && isgraphics(args{1})
    parent = args{1};
    args = args(2:end);
  end
  if mod(numel(args), 2) ~= 0
    error('Nelson:uicomponent:pvPairsExpected', 'Wrong number of input arguments: property/value pairs expected.');
  end
  if isempty(parent)
    parent = uifigure();
  end
  userSetsPosition = false;
  for k = 1:2:numel(args)
    name = lower(char(args{k}));
    if strcmp(name, 'position') || strcmp(name, 'innerposition') || strcmp(name, 'outerposition')
      userSetsPosition = true;
    end
  end
  ax = __uiaxes__('Parent', parent, 'NextPlot', 'replacechildren', 'FontUnits', 'pixels', 'FontSize', 12);
  ax.Units = 'pixels';
  if ~userSetsPosition
    ax.Position = [10 10 400 300];
  end
  for k = 1:2:numel(args)
    set(ax, args{k}, args{k + 1});
  end
end
%=============================================================================
