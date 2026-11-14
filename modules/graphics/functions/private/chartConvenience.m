%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [handled, output] = chartConvenience(name, args)
  % Convenience functions on a heatmap: titles, labels, colormap, color limits and
  % grid act on the chart; functions that need an axes are refused.
  handled = false;
  output = [];
  [h, args] = targetedHeatmap(args);
  if isempty(h)
    return
  end
  handled = true;
  switch name
    case 'title'
      setChartText(h, 'Title', args);
    case 'xlabel'
      setChartText(h, 'XLabel', args);
    case 'ylabel'
      setChartText(h, 'YLabel', args);
    case 'grid'
      setChartGrid(h, args);
    case 'colormap'
      output = chartColormap(h, args);
    case {'clim', 'caxis'}
      output = chartColorLimits(h, name, args);
    otherwise
      unsupported(name);
  end
end
%=============================================================================
function [h, args] = targetedHeatmap(args)
  % the heatmap given first, else the chart that is the current axes
  h = [];
  if ~isempty(args) && ~isnumeric(args{1}) && strncmp(class(args{1}), 'nelson.', 7)
    if isscalar(args{1}) && isgraphics(args{1}, 'heatmap')
      h = args{1};
      args = args(2:end);
    end
    return
  end
  fig = get(groot, 'CurrentFigure');
  if isempty(fig)
    return
  end
  current = fig.CurrentAxes;
  if isscalar(current) && isgraphics(current, 'heatmap')
    h = current;
  end
end
%=============================================================================
function setChartText(h, name, args)
  if isempty(args)
    error('Nelson:notEnoughInputs', _('Not enough input arguments.'));
  end
  set(h, name, args{1});
end
%=============================================================================
function setChartGrid(h, args)
  if isempty(args)
    state = 'on';
    if strcmp(char(h.GridVisible), 'on')
      state = 'off';
    end
  elseif ischar(args{1}) && any(strcmpi(args{1}, {'on', 'off'}))
    state = lower(args{1});
  else
    unsupported('grid');
  end
  h.GridVisible = state;
end
%=============================================================================
function map = chartColormap(h, args)
  if ~isempty(args)
    map = args{1};
    if ischar(map)
      map = feval(map);
    end
    h.Colormap = map;
  end
  map = h.Colormap;
end
%=============================================================================
function limits = chartColorLimits(h, name, args)
  if ~isempty(args)
    if ~isnumeric(args{1})
      unsupported(name);
    end
    h.ColorLimits = args{1};
  end
  limits = h.ColorLimits;
end
%=============================================================================
function unsupported(name)
  error('Nelson:heatmap:unsupportedFunction', _('Using %s with heatmap is not supported.'), name);
end
%=============================================================================
