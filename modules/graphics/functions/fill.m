%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fill(varargin)
  % fill(X, Y, C)
  % fill(X1, Y1, C1, ..., Xn, Yn, Cn)
  % fill(..., Name, Value)
  % fill(ax, ...)
  % p = fill(...)

  nargoutchk(0, 1);
  narginchk(3, 1000);

  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = parseParent(inputArguments);
  ax = ancestor(parent, 'axes');
  nextPlot = '';
  if ~isempty(ax) && isscalar(ax) && isgraphics(ax, 'axes')
    nextPlot = ax.NextPlot;
  end
  [groups, propertiesList] = parseFillGroups(inputArguments, 3);

  h = gobjects(1, 0);
  for k = 1:length(groups)
    group = groups{k};
    h = [h, renderFillGroup(parent, group{1}, group{2}, group{3}, propertiesList)];
  end
  if ~isempty(ax) && any(strcmp(nextPlot, {'replace', 'replaceall'}))
    ax.Box = 'on';
  end

  if (nargout > 0)
    varargout{1} = h;
  else
    varargout = {};
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
function [groups, propertiesList] = parseFillGroups(inputArguments, groupWidth)
  groups = {};
  k = 1;
  while k <= length(inputArguments)
    if isPatchPropertyName(inputArguments{k})
      break
    end
    if k + groupWidth - 1 > length(inputArguments)
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
    end
    groups{end + 1} = inputArguments(k:k + groupWidth - 1);
    k = k + groupWidth;
  end
  if isempty(groups)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  propertiesList = inputArguments(k:end);
  if mod(length(propertiesList), 2) ~= 0
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function h = renderFillGroup(parent, XData, YData, CData, propertiesList)
  [XData, YData] = normalizeFillMatrices(XData, YData);
  nbPolygons = size(XData, 2);
  h = gobjects(1, nbPolygons);
  for k = 1:nbPolygons
    X = XData(:, k);
    Y = YData(:, k);
    Z = zeros(size(X));
    C = selectColorData(CData, k, nbPolygons);
    h(k) = patch(parent, X, Y, Z, C, propertiesList{:});
  end
end
%=============================================================================
function [XData, YData] = normalizeFillMatrices(XData, YData)
  if isvector(XData)
    XData = XData(:);
  end
  if isvector(YData)
    YData = YData(:);
  end
  if size(XData, 1) == 1 && size(YData, 1) > 1
    XData = XData.';
  end
  if size(YData, 1) == 1 && size(XData, 1) > 1
    YData = YData.';
  end
  if size(XData, 2) == 1 && size(YData, 2) > 1
    XData = repmat(XData, 1, size(YData, 2));
  end
  if size(YData, 2) == 1 && size(XData, 2) > 1
    YData = repmat(YData, 1, size(XData, 2));
  end
  if ~isequal(size(XData), size(YData))
    error('Nelson:dimagree:sizeDataMismatch', 'Size of data mismatch.');
  end
end
%=============================================================================
function C = selectColorData(CData, columnIndex, nbPolygons)
  C = CData;
  if ischar(CData) || isStringScalar(CData)
    return
  end
  if isnumeric(CData) && isvector(CData) && numel(CData) == 3
    C = CData(:).';
    return
  end
  if isnumeric(CData) && ismatrix(CData)
    if size(CData, 2) == nbPolygons && size(CData, 1) ~= 3
      C = CData(:, columnIndex);
    elseif size(CData, 3) == 1 && size(CData, 2) == nbPolygons && size(CData, 1) == 3
      C = CData(:, columnIndex);
    end
  end
end
%=============================================================================
function tf = isPatchPropertyName(value)
  tf = graphicsIsPropertyNameToken(value, 'patch');
end
%=============================================================================
