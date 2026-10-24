%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = compassplot(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseCompassParent(args);
  [theta, radius, source, args] = parseCompassData(args);
  props = parseCompassProperties(args);
  [fig, previousDrawLater] = polarBeginDrawLater(ax);
  try
    ax = polarPrepareDataAxes(ax);
    [h, allHandles] = drawCompassVectors(ax, theta, radius, source, props);
    polarAppendDataHandles(ax, allHandles);
    polarEndDrawLater(fig, previousDrawLater);
  catch exception
    polarEndDrawLater(fig, previousDrawLater);
    error(exception.message);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [ax, args] = parseCompassParent(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1})
    if ~isgraphics(args{1}, 'polaraxes')
      error('Nelson:graphics:invalidParentInput', 'Parent input must be a polaraxes object.');
    end
    ax = args{1};
    args = args(2:end);
    if isempty(args)
      error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Value');
    end
    return
  end
  [ax, args] = polarParseTargetAxes(args);
end
%=============================================================================
function [theta, radius, source, args] = parseCompassData(args)
  source = struct('Table', [], 'ThetaVariable', [], 'RVariable', []);
  if isempty(args)
    error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Value');
  end
  if (istable(args{1}) || istimetable(args{1})) && numel(args) >= 3
    [theta, radius, source] = parseCompassTableData(args);
    [theta, radius] = matchCompassData(theta, radius);
    args = args(4:end);
  elseif istable(args{1}) || istimetable(args{1})
    error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Value');
  elseif isnumeric(args{1}) && (numel(args) == 1 || isCompassPropertyName(args{2}))
    z = args{1};
    theta = angle(z);
    radius = abs(z);
    args = args(2:end);
  elseif numel(args) >= 2 && isnumeric(args{1}) && isnumeric(args{2})
    theta = args{1};
    radius = args{2};
    [theta, radius] = matchCompassData(theta, radius);
    args = args(3:end);
  else
    error('Nelson:compassplot:numericVectorDataExpected', '%s must be a numeric vector.', 'Value');
  end
  if ~(isnumeric(theta) && isnumeric(radius)) || ~isreal(theta) || ~isreal(radius)
    error('Nelson:compassplot:numericRealVectorExpected', 'Numeric real vector data expected.');
  end
end
%=============================================================================
function [theta, radius, source] = parseCompassTableData(args)
  tbl = args{1};
  thetaNames = graphicsTableVariableNames(tbl, args{2});
  radiusNames = graphicsTableVariableNames(tbl, args{3});
  nSeries = max(numel(thetaNames), numel(radiusNames));
  validateTableVariableCounts(thetaNames, radiusNames, nSeries);
  [theta, radius, thetaVars, radiusVars] = compassTableSeries(tbl, thetaNames, radiusNames, nSeries);
  source = struct('Table', tbl, 'ThetaVariable', {thetaVars}, 'RVariable', {radiusVars});
end
%=============================================================================
function validateTableVariableCounts(thetaNames, radiusNames, nSeries)
  counts = [numel(thetaNames), numel(radiusNames)];
  if any(counts ~= 1 & counts ~= nSeries)
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
end
%=============================================================================
function [theta, radius, thetaVars, radiusVars] = compassTableSeries(tbl, thetaNames, radiusNames, nSeries)
  theta = [];
  radius = [];
  thetaVars = cell(1, nSeries);
  radiusVars = cell(1, nSeries);
  for k = 1:nSeries
    thetaName = selectName(thetaNames, k);
    radiusName = selectName(radiusNames, k);
    theta = appendColumn(theta, numericColumn(tbl.(thetaName), thetaName));
    radius = appendColumn(radius, numericColumn(tbl.(radiusName), radiusName));
    thetaVars{k} = thetaName;
    radiusVars{k} = radiusName;
  end
end
%=============================================================================
function [theta, radius] = matchCompassData(theta, radius)
  if isvector(theta) && isvector(radius)
    if numel(theta) ~= numel(radius)
      error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'Theta', 'R');
    end
    theta = theta(:);
    radius = radius(:);
    return
  end
  if isvector(theta) && ismatrix(radius)
    theta = expandCompassVector(theta, size(radius));
    return
  end
  if ismatrix(theta) && isvector(radius)
    radius = expandCompassVector(radius, size(theta));
    return
  end
  if ~isequal(size(theta), size(radius))
    error('Nelson:dimagree:thetaRCompatibleSizes', 'Theta and R must have compatible sizes.');
  end
end
%=============================================================================
function data = appendColumn(data, value)
  value = value(:);
  if isempty(data)
    data = value;
  else
    data = [data, value];
  end
end
%=============================================================================
function value = numericColumn(value, name)
  if ~isnumeric(value)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', name);
  end
  value = value(:);
end
%=============================================================================
function name = selectName(names, k)
  if numel(names) == 1
    name = names{1};
  else
    name = names{k};
  end
end
%=============================================================================
function values = expandCompassVector(values, targetSize)
  n = numel(values);
  if n == targetSize(1)
    values = repmat(values(:), 1, targetSize(2));
  elseif n == targetSize(2)
    values = repmat(values(:).', targetSize(1), 1);
  else
    error('Nelson:dimagree:thetaRCompatibleSizes', 'Theta and R must have compatible sizes.');
  end
end
%=============================================================================
function props = parseCompassProperties(args)
  props = {};
  if isempty(args)
    return
  end
  if numel(args) == 1 && (ischar(args{1}) || isStringScalar(args{1}))
    props = {'Color', validatecolor(args{1})};
    return
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = char(args{k});
    value = args{k + 1};
    switch lower(name)
      case {'color', 'linewidth', 'linestyle', 'displayname', 'visible', 'tag', 'userdata', ...
          'thetadatasource', 'rdatasource'}
        props = [props, {name, value}];
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'compassplot');
    end
  end
end
%=============================================================================
function tf = isCompassPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  names = {'Color', 'LineWidth', 'LineStyle', 'DisplayName', 'Visible', 'Tag', 'UserData', ...
    'ThetaDataSource', 'RDataSource'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
function [mainHandles, allHandles] = drawCompassVectors(ax, theta, radius, source, props)
  if isvector(theta)
    theta = theta(:);
    radius = radius(:);
  end
  mainHandles = gobjects(size(theta, 2), 1);
  allHandles = gobjects(0, 1);
  for k = 1:size(theta, 2)
    color = getColorAndUpdateIndex(ax);
    vectorProps = [{'Color', color}, props];
    sourceProps = {};
    if ~isempty(source.Table)
      sourceProps = {'SourceTable', source.Table, 'ThetaVariable', selectSourceName(source.ThetaVariable, k), ...
        'RVariable', selectSourceName(source.RVariable, k)};
    end
    mainHandles(k) = __compassplot__('Parent', ax, 'XData', [], 'YData', [], ...
      'ThetaData', theta(:, k), 'RData', radius(:, k), sourceProps{:}, vectorProps{:});
    allHandles = [allHandles; mainHandles(k)];
  end
end
%=============================================================================
function name = selectSourceName(names, k)
  if iscell(names)
    name = selectName(names, k);
  else
    name = names;
  end
end
%=============================================================================
