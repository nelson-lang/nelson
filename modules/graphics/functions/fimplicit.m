%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fimplicit(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 2);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  if isempty(args) || ~(isa(args{1}, 'function_handle') || isFunctionHandleCell(args{1}))
    error('Nelson:graphics:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  functions = normalizeFunctions(args{1});
  args = args(2:end);
  [styleProperties, args] = parseLeadingLineSpec(args);
  [range, rangeSet, args] = parseRange(args);
  [moreStyleProperties, args] = parseLeadingLineSpec(args);
  styleProperties = [styleProperties, moreStyleProperties];
  objectProperties = parseObjectProperties(args);
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'axes')
    ax = newplot(parent);
  else
    ax = ancestor(parent, 'axes');
  end
  nextPlot = ax.NextPlot;
  rangeProperties = {};
  if rangeSet
    rangeProperties = {'XRange', range(1:2), 'YRange', range(3:4)};
  end
  colorIsUserDefined = hasColorProperty(styleProperties) || hasColorProperty(objectProperties);
  h = gobjects(numel(functions), 1);
  for k = 1:numel(functions)
    colorProperties = {};
    if ~colorIsUserDefined
      colorProperties = {'Color', getColorAndUpdateIndex(ax)};
    end
    h(k) = __fimplicit__('Parent', ax, 'Function', functions{k}, rangeProperties{:}, ...
      styleProperties{:}, colorProperties{:}, objectProperties{:});
  end
  axis(ax, 'tight');
  if rangeSet && range(1) < range(2) && range(3) < range(4)
    ax.XLim = range(1:2);
    ax.YLim = range(3:4);
  end
  if any(strcmp(nextPlot, {'replace', 'replaceall'}))
    ax.Box = 'on';
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function tf = isFunctionHandleCell(value)
  tf = iscell(value) && all(cellfun(@(f) isa(f, 'function_handle'), value(:)));
end
%=============================================================================
function functions = normalizeFunctions(value)
  if iscell(value)
    functions = value(:);
  else
    functions = {value};
  end
end
%=============================================================================
function [range, rangeSet, args] = parseRange(args)
  range = [-5 5 -5 5];
  rangeSet = false;
  if ~isempty(args) && (isa(args{1}, 'function_handle') || isFunctionHandleCell(args{1}))
    error('Nelson:graphics:tooManyFunctions', 'Too many functions.');
  end
  if ~isempty(args) && isnumeric(args{1}) && isreal(args{1}) ...
      && (numel(args{1}) == 2 || numel(args{1}) == 4)
    range = normalizeRange(args{1});
    rangeSet = true;
    args(1) = [];
  elseif ~isempty(args) && isnumeric(args{1})
    error('Nelson:graphics:invalidRange', 'Range must have two or four elements.');
  end
end
%=============================================================================
function range = normalizeRange(value)
  value = double(value(:)');
  if numel(value) == 2
    range = [value(1), value(2), value(1), value(2)];
  elseif numel(value) == 4
    range = value;
  else
    error('Nelson:graphics:invalidRange', 'Range must have two or four elements.');
  end
  if range(1) > range(2) || range(3) > range(4) || any(~isfinite(range))
    error('Nelson:graphics:rangeNotIncreasing', 'Range values must be increasing and finite.');
  end
end
%=============================================================================
function [styleProperties, args] = parseLeadingLineSpec(args)
  styleProperties = {};
  if isempty(args) || ~(ischar(args{1}) || isStringScalar(args{1}))
    return
  end
  [lineStyle, color, marker, msg] = colstyle(args{1}, 'plot', false);
  if ~isempty(msg)
    return
  end
  args(1) = [];
  if ~isempty(lineStyle)
    styleProperties = [styleProperties, {'LineStyle', lineStyle}];
  elseif ~isempty(marker)
    styleProperties = [styleProperties, {'LineStyle', 'none'}];
  end
  if ~isempty(color)
    styleProperties = [styleProperties, {'Color', color}];
  end
  if ~isempty(marker)
    styleProperties = [styleProperties, {'Marker', marker}];
  end
end
%=============================================================================
function properties = parseObjectProperties(args)
  properties = args;
  if isempty(properties)
    return
  end
  if mod(numel(properties), 2) ~= 0
    if (ischar(properties{1}) || isStringScalar(properties{1})) ...
        && ~isFimplicitPropertyName(properties{1})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(properties{1}));
    end
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(properties)
    if ~isFimplicitPropertyName(properties{k})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(properties{k}));
    end
  end
end
%=============================================================================
function tf = hasColorProperty(properties)
  tf = false;
  for k = 1:2:numel(properties)
    if (ischar(properties{k}) || isStringScalar(properties{k})) ...
        && strcmpi(char(properties{k}), 'Color')
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = isFimplicitPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  name = lower(char(value));
  names = {'annotation', 'beingdeleted', 'busyaction', 'buttondownfcn', 'children', ...
    'clipping', 'color', 'contextmenu', 'createfcn', 'deletefcn', 'displayname', ...
    'function', 'handlevisibility', 'hittest', 'interruptible', 'linestyle', ...
    'linewidth', 'marker', 'meshdensity', 'parent', 'pickableparts', 'selected', ...
    'selectionhighlight', 'tag', 'userdata', 'visible', 'xrange', 'xrangemode', ...
    'yrange', 'yrangemode'};
  tf = any(strcmp(name, names));
end
%=============================================================================
