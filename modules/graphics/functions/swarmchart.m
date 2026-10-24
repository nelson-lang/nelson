%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = swarmchart(varargin)
  narginchk(2, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [x, y, args] = parseSwarmData(args);
  [opts, scatterArgs] = parseSwarmOptions(args);
  ax = graphicsTargetAxes(parent);
  h = scatter(ax, x(:), y(:), scatterArgs{:});
  h.XJitter = opts.XJitter;
  h.XJitterWidth = swarmJitterWidth(x, opts.XJitterWidth);
  expandSwarmJitterLimit(ax, x, opts.XJitter, h.XJitterWidth, 'X');
  h.UserData = struct('ChartType', 'swarmchart', 'XDataOriginal', x, ...
    'YDataOriginal', y, 'Options', opts);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [x, y, args] = parseSwarmData(args)
  if (istable(args{1}) || istimetable(args{1})) && numel(args) >= 3
    x = graphicsTableVariable(args{1}, args{2});
    y = graphicsTableVariable(args{1}, args{3});
    args = args(4:end);
  elseif numel(args) >= 2
    x = args{1};
    y = args{2};
    args = args(3:end);
  else
    error('Nelson:swarmchart:xAndYExpected', 'X and Y data expected.');
  end
  if ~isnumeric(x) || ~isnumeric(y) || numel(x) ~= numel(y)
    error('Nelson:swarmchart:invalidXAndY', 'X and Y must be numeric arrays with the same number of elements.');
  end
end
%=============================================================================
function [opts, scatterArgs] = parseSwarmOptions(args)
  opts = struct('XJitter', 'density', 'XJitterWidth', [], 'ColorVariable', []);
  scatterArgs = {};
  k = 1;
  while k <= length(args)
    if ischar(args{k}) && k < length(args)
      name = lower(args{k});
      value = args{k + 1};
      switch name
        case 'xjitter'
          opts.XJitter = lower(char(value));
          k = k + 2;
          continue
        case 'xjitterwidth'
          opts.XJitterWidth = double(value);
          k = k + 2;
          continue
        case 'colorvariable'
          opts.ColorVariable = value;
          k = k + 2;
          continue
      end
    end
    scatterArgs{end + 1} = args{k};
    k = k + 1;
  end
end
%=============================================================================
function width = swarmJitterWidth(x, requestedWidth)
  if ~isempty(requestedWidth)
    width = requestedWidth;
    return
  end
  levels = unique(double(x(:).'));
  if numel(levels) <= 1
    width = 0.9;
  else
    width = 0.9 * min(diff(levels));
  end
end
%=============================================================================
function expandSwarmJitterLimit(ax, values, jitterMode, jitterWidth, axisName)
  % The jitter spread is applied at render time, so the axis auto limits
  % (computed from the un-jittered category positions) would clip the
  % spread points. Widen the jittered axis by half the category spacing so
  % that the whole swarm stays visible.
  if strcmp(jitterMode, 'none')
    return
  end
  v = double(values(:).');
  v = v(isfinite(v));
  if isempty(v)
    return
  end
  levels = unique(v);
  if numel(levels) <= 1
    spacing = 1;
  else
    spacing = min(diff(levels));
  end
  margin = max(0.5 * spacing, 0.5 * jitterWidth);
  lo = min(levels) - margin;
  hi = max(levels) + margin;
  limName = [axisName, 'Lim'];
  current = get(ax, limName);
  set(ax, limName, [min(current(1), lo), max(current(2), hi)]);
end
%=============================================================================
