%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = scatter(varargin)
  % scatter(x, y)
  % scatter(x, y, s)
  % scatter(x, y, s, c)
  % scatter(..., 'filled')
  % scatter(..., mkr)
  % scatter(ax, ...)
  % scatter(..., Name, Value)
  % s = scatter(...)
  narginchk(2, 100);
  nargoutchk(0, 1);
  
  [parent, X, Y, filled, marker, markerSize, markerColor, otherProperties] = parseArguments(varargin);
  
  if ~isequal(size(X), size(Y))
    error (_('X and Y must have the same size.'));
  end
  if (~isscalar(markerSize) &&  ~isequal(size(X), size(markerSize)))
    error (_('size of S must match X, Y.'));
  end
  if (size(markerColor, 1) > 1 && size(markerColor, 1) ~= size(X, 1))
    error(_('number of colors in C must match number of points in X.'));
  end
  
  if isscalar(markerSize) && (isscalar(markerColor) || isempty(markerColor)) 
    if isvector(X) && isvector(Y)
      if isempty(markerColor)
        markerColor = getColorAndUpdateIndex(parent);
      end
      h = scatterPlot(parent, X, Y, markerColor, markerSize, marker, filled, otherProperties);
    else
      h = [];
      for k = 1:size(X, 2)
        if isempty(markerColor)
          color = getColorAndUpdateIndex(parent);
        else
          color = markerColor;
        end
        go = scatterPlot(parent, X(:, k), Y(:, k), color, markerSize, marker, filled, otherProperties);
        h = [h, go];
      end    
    end
  else
    nbElements = length(X);
    if isscalar(markerSize)
      markerSize = ones (1, nbElements) * markerSize;
    end
    cmap = colormap(parent);
    
    group = hggroup('Parent', parent);
    group.Visible = 'off';
    
    xLimMode = xlim('mode');
    yLimMode = ylim('mode');
    parent.XLim = [min(X), max(X)];
    parent.YLim = [min(Y), max(Y)];
    
    minMarkerColor = min(markerColor);
    maxMarkerColor = max(markerColor);
    lengthColormap = (length(cmap) - 1);
    
    for k = 1:length(X)
      if isscalar(markerColor) || (isvector(markerColor) && length(markerColor) == 3)
        color = markerColor;
      else
        index = round((markerColor(k) - minMarkerColor) / (maxMarkerColor - minMarkerColor) * lengthColormap) + 1;
        color = cmap(index, :);
      end
      scatterPlot(group, X(k), Y(k), color, markerSize(k), marker, filled, otherProperties);
    end
    parent.XLimMode = 'auto';
    parent.YLimMode = 'auto';          
    group.Visible = 'on';
    h = group;      
  end
  
  if (nargout == 1)
    varargout{1} = h;
  end
end
%=============================================================================
function h = scatterPlot(parent, x, y, color, markerSize, marker, filled, otherProperties)
  
  markerFaceColor = 'none';
  if filled
    markerFaceColor = color;
  end
  scatterProperties = struct('Marker', marker, ... 
  'LineStyle', 'none', ...
  'Color', color, ...
  'MarkerEdgeColor', color, ...
  'MarkerSize', markerSize, ...
  'MarkerFaceColor', markerFaceColor);
  
  for name = fieldnames(otherProperties)'
    scatterProperties.(name{1}) = otherProperties.(name{1});
  end
  
  scatterProperties = reshape([fieldnames(scatterProperties)'; struct2cell(scatterProperties)'], 1, []);
  h = line(parent, x, y, scatterProperties{:});
end
%=============================================================================
function [parent, X, Y, filled, marker, markerSize, markerColor, otherProperties] = parseArguments(inputArguments)
  markerSize = sqrt(36);
  marker = 'o';
  markerColor = [];
  filled = false;
  nbInputArguments = length(inputArguments);
  if (nbInputArguments >= 1)
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
      parent = inputArguments{1}(1);
      inputArguments = inputArguments(2:end);
      nbInputArguments = nbInputArguments - 1;
    else   
      parent = newplot();
    end
  else
    parent = newplot();
  end
  
  isFilled = @(x) (ischar(x) || isStringScalar(x)) && strcmp(convertStringsToChars(x), 'filled');
  filledDectected = find (cellfun(isFilled, inputArguments));
  if (~isempty(filledDectected))
    filled = true;
    inputArguments(filledDectected) = [];
    nbInputArguments = length(inputArguments);
  end
  
  supportedMarkers = string(getMarkerNameList());
  isMarker = @(x) (ischar(x) || isStringScalar(x)) && matches(convertCharsToStrings(x), supportedMarkers);
  markerDetected = find (cellfun(isMarker, inputArguments));
  if (~isempty(markerDetected))
    marker = inputArguments{markerDetected(end)};
    inputArguments(markerDetected) = [];
    nbInputArguments = length(inputArguments);
  end
  
  supportedColorString = [string(getColorShortNameList()), string(getColorNameList())];
  isString = @(x) (ischar(x) || isStringScalar(x)) && ~matches(convertCharsToStrings(x), supportedColorString);
  firstString = find (cellfun(isString, inputArguments), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  if (firstString > 4)
    X = inputArguments{1};
    Y = inputArguments{2};
    if ~isempty(inputArguments{3})
      markerSize = sqrt(inputArguments{3});
    end
    markerColor = getColorShortName(inputArguments{4});
    inputArguments = inputArguments(5:end);
    nbInputArguments = nbInputArguments - 4;
  elseif (firstString > 3)
    X = inputArguments{1};
    Y = inputArguments{2};
    if ~isempty(inputArguments{3})
      markerSize = sqrt(inputArguments{3});
    end
    inputArguments = inputArguments(4:end);
    nbInputArguments = nbInputArguments - 3;
  elseif (firstString > 2)
    X = inputArguments{1};
    Y = inputArguments{2};
    inputArguments = inputArguments(3:end);
    nbInputArguments = nbInputArguments - 2;
  end
  otherProperties = struct(inputArguments{:});
end
%=============================================================================
