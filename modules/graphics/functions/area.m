%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = area(varargin)
  % area(Y)
  % area(X, Y)
  % area(..., basevalue)
  % area(..., Name, Value)
  % area(ax, ...)
  % a = area(...)

  narginchk(1, 1000);
  nargoutchk(0, 1);

  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = parseParent(inputArguments);
  ax = areaAxes(parent);
  nextPlot = '';
  if ~isempty(ax) && isscalar(ax) && isgraphics(ax, 'axes')
    nextPlot = ax.NextPlot;
  end
  [XData, YData, baseValue, propertiesList, xAuto, storeRow] = parseAreaArguments(inputArguments);
  [XData, xIsDate] = datetimeToSerial(XData);
  [YData, yIsDate] = datetimeToSerial(YData);
  h = areaPlot(parent, XData, YData, baseValue, propertiesList, xAuto, storeRow);
  fixAreaAxesLimits(parent, XData);
  if ~isempty(ax) && any(strcmp(nextPlot, {'replace', 'replaceall'}))
    ax.Box = 'on';
  end
  if xIsDate
    applyDatetimeAxis(parent, 'x');
  end
  if yIsDate
    applyDatetimeAxis(parent, 'y');
  end

  if nargout > 0
    varargout{1} = h;
  else
    varargout = {};
  end
end
%=============================================================================
function fixAreaAxesLimits(parent, XData)
  ax = areaAxes(parent);
  if isempty(ax) || ~strcmp(ax.XLimMode, 'auto')
    return
  end
  xLimits = [min(XData(:)), max(XData(:))];
  if all(isfinite(xLimits)) && xLimits(2) > xLimits(1)
    ax.XLim = xLimits;
  end
end
%=============================================================================
function ax = areaAxes(parent)
  if isgraphics(parent, 'axes')
    ax = parent;
  else
    ax = ancestor(parent, 'axes');
  end
end
%=============================================================================
function [parent, inputArguments] = parseParent(inputArguments)
  if ~isempty(inputArguments) && isscalar(inputArguments{1}) ...
      && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup'))
    parent = inputArguments{1}(1);
    inputArguments = inputArguments(2:end);
    parent = newplotTarget(parent);
  else
    [parent, inputArguments] = graphicsExtractParentProperty(inputArguments);
    parent = newplotTarget(parent);
  end
end
%=============================================================================
function [XData, YData, baseValue, propertiesList, xAuto, storeRow] = parseAreaArguments(inputArguments)
  firstProperty = findFirstAreaProperty(inputArguments);
  propertiesList = inputArguments(firstProperty:end);
  dataArguments = inputArguments(1:firstProperty - 1);
  baseValue = 0;
  if ~isempty(dataArguments) && isnumeric(dataArguments{end}) && isscalar(dataArguments{end}) ...
      && length(dataArguments) > 1
    baseValue = dataArguments{end};
    dataArguments = dataArguments(1:end - 1);
  end
  % xAuto: X was generated from indices (single data argument) -> XDataMode 'auto'.
  % storeRow: a single vector series keeps the input row orientation on read-back.
  xAuto = false;
  storeRow = false;
  if length(dataArguments) == 1
    YData = dataArguments{1};
    xAuto = true;
    if isvector(YData)
      storeRow = isrow(YData);
      YData = YData(:);
    end
    XData = (1:size(YData, 1)).';
  elseif length(dataArguments) == 2
    XData = dataArguments{1};
    YData = dataArguments{2};
    if isvector(XData) && isvector(YData)
      storeRow = isrow(YData);
    end
  elseif isempty(dataArguments)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  else
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if isvector(YData)
    YData = YData(:);
  end
  if isvector(XData)
    XData = XData(:);
  end
  if size(XData, 2) == 1 && size(YData, 2) > 1
    XData = repmat(XData, 1, size(YData, 2));
  end
  if ~isequal(size(XData), size(YData))
    error('Nelson:dimagree:sizeDataMismatch', 'Size of data mismatch.');
  end
  if mod(length(propertiesList), 2) ~= 0
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function index = findFirstAreaProperty(inputArguments)
  index = length(inputArguments) + 1;
  for k = 1:length(inputArguments)
    if isAreaPropertyName(inputArguments{k})
      index = k;
      return
    end
  end
end
%=============================================================================
function h = areaPlot(parent, XData, YData, baseValue, propertiesList, xAuto, storeRow)
  lower = baseValue * ones(size(YData, 1), 1);
  h = gobjects(1, size(YData, 2));
  for k = 1:size(YData, 2)
    upper = lower + YData(:, k);
    X = XData(:, k);
    vertices = [X, upper, ones(size(X)); flipud(X), flipud(lower), ones(size(X))];
    faces = 1:size(vertices, 1);
    color = getColorAndUpdateIndex(parent);
    % Preserve the input orientation for a single vector series: the reference
    % reads XData/YData back as a row when the data was given as a row.
    if storeRow
      xStore = X.';
      yStore = YData(:, k).';
    else
      xStore = X;
      yStore = YData(:, k);
    end
    % Default edge is the dark-gray axes foreground (33/255), matching the
    % reference and bar/surf -- not pure black.
    args = {'Parent', parent, 'XData', xStore, 'YData', yStore, ...
      'Faces', faces, 'Vertices', vertices, 'FaceColor', color, ...
      'EdgeColor', [0.129411764705882, 0.129411764705882, 0.129411764705882], ...
      'BaseValue', baseValue};
    args = [args, propertiesList];
    h(k) = __area__(args{:});
    % Setting XData flips XDataMode to 'manual'; when X was the auto sample
    % index, restore 'auto' afterwards to match the reference.
    if xAuto
      h(k).XDataMode = 'auto';
    end
    lower = upper;
  end
end
%=============================================================================
function tf = isAreaPropertyName(value)
  tf = ischar(value) && isValidGraphicsProperty('area', value);
end
%=============================================================================
