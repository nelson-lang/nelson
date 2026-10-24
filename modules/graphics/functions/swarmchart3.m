%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = swarmchart3(varargin)
  narginchk(3, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [x, y, z, args] = parseSwarm3Data(args);
  [opts, scatterArgs] = parseSwarm3Options(args);
  ax = graphicsTargetAxes(parent);
  h = scatter3(ax, x(:), y(:), z(:), scatterArgs{:});
  h.XJitter = opts.XJitter;
  h.XJitterWidth = swarmJitterWidth(x, opts.XJitterWidth);
  h.YJitter = opts.YJitter;
  h.YJitterWidth = swarmJitterWidth(y, opts.YJitterWidth);
  expandSwarmJitterLimit(ax, x, opts.XJitter, h.XJitterWidth, 'X');
  expandSwarmJitterLimit(ax, y, opts.YJitter, h.YJitterWidth, 'Y');
  view(ax, 3);
  h.UserData = struct('ChartType', 'swarmchart3', 'XDataOriginal', x, ...
    'YDataOriginal', y, 'ZDataOriginal', z, 'Options', opts);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [x, y, z, args] = parseSwarm3Data(args)
  if (istable(args{1}) || istimetable(args{1})) && numel(args) >= 4
    x = graphicsTableVariable(args{1}, args{2});
    y = graphicsTableVariable(args{1}, args{3});
    z = graphicsTableVariable(args{1}, args{4});
    args = args(5:end);
  elseif numel(args) >= 3
    x = args{1};
    y = args{2};
    z = args{3};
    args = args(4:end);
  else
    error('Nelson:swarmchart3:xYAndExpected', 'X, Y, and Z data expected.');
  end
  if ~isnumeric(x) || ~isnumeric(y) || ~isnumeric(z) || numel(x) ~= numel(y) || numel(x) ~= numel(z)
    error('Nelson:swarmchart3:invalidXYAnd', 'X, Y, and Z must be numeric arrays with the same number of elements.');
  end
end
%=============================================================================
function [opts, scatterArgs] = parseSwarm3Options(args)
  opts = struct('XJitter', 'density', 'XJitterWidth', [], ...
    'YJitter', 'density', 'YJitterWidth', [], 'ColorVariable', []);
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
        case 'yjitter'
          opts.YJitter = lower(char(value));
          k = k + 2;
          continue
        case 'yjitterwidth'
          opts.YJitterWidth = double(value);
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
function width = swarmJitterWidth(values, requestedWidth)
  if ~isempty(requestedWidth)
    width = requestedWidth;
    return
  end
  levels = unique(double(values(:).'));
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
