%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fill3(varargin)
  % fill3(X, Y, Z, C)
  % fill3(X1, Y1, Z1, C1, ..., Xn, Yn, Zn, Cn)
  % fill3(..., Name, Value)
  % fill3(ax, ...)
  % p = fill3(...)

  nargoutchk(0, 1);
  narginchk(4, 1000);

  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = parseParent(inputArguments);
  ax = parent;
  if isgraphics(parent, 'hggroup') || isgraphics(parent, 'hgtransform')
    ax = ancestor(parent, 'axes');
  end
  nextplot = ax.NextPlot;
  [groups, propertiesList] = parseFillGroups(inputArguments, 4);

  h = gobjects(1, 0);
  for k = 1:length(groups)
    group = groups{k};
    h = [h, renderFill3Group(parent, group{1}, group{2}, group{3}, group{4}, propertiesList)];
  end

  if strcmp(nextplot, 'replaceall') || strcmp(nextplot, 'replace')
    view(ax, 3);
    grid(ax, 'off');
  elseif strcmp(nextplot, 'replacechildren')
    view(ax, 3);
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
      && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') ...
      || isgraphics(inputArguments{1}, 'hgtransform'))
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
function h = renderFill3Group(parent, XData, YData, ZData, CData, propertiesList)
  [XData, YData, ZData] = normalizeFill3Matrices(XData, YData, ZData);
  nbPolygons = size(XData, 2);
  h = gobjects(1, nbPolygons);
  for k = 1:nbPolygons
    C = selectColorData(CData, k, nbPolygons);
    h(k) = patch(parent, XData(:, k), YData(:, k), ZData(:, k), C, propertiesList{:});
  end
end
%=============================================================================
function [XData, YData, ZData] = normalizeFill3Matrices(XData, YData, ZData)
  if isvector(XData), XData = XData(:); end
  if isvector(YData), YData = YData(:); end
  if isvector(ZData), ZData = ZData(:); end
  widths = [size(XData, 2), size(YData, 2), size(ZData, 2)];
  nbPolygons = max(widths);
  if size(XData, 2) == 1 && nbPolygons > 1, XData = repmat(XData, 1, nbPolygons); end
  if size(YData, 2) == 1 && nbPolygons > 1, YData = repmat(YData, 1, nbPolygons); end
  if size(ZData, 2) == 1 && nbPolygons > 1, ZData = repmat(ZData, 1, nbPolygons); end
  if ~isequal(size(XData), size(YData)) || ~isequal(size(XData), size(ZData))
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
  tf = ischar(value) && isValidGraphicsProperty('patch', value);
end
%=============================================================================
