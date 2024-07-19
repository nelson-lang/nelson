%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stem(varargin)
  % h = stem(Y)
  % h = stem(X, Y)
  % h = stem(ax, Y)
  % h = stem(X, Y, LineSpec)
  % h = stem(ax, X, Y, LineSpec)
  nargoutchk(0, 1);
  narginchk(1, 20);
  
  [parent, X, Y, filled, propertiesLineA, propertiesLineB] = parseArguments(varargin);
  n =  size(Y, 2);
  G = [];
  
  if isgraphics(parent, 'hggroup') 
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
  colorOrder = ax.ColorOrder;
  colorIndex = ax.ColorOrderIndex;
  for k = 1:n 
    nbColors = size (colorOrder, 1);
    colorIndex = mod (colorIndex, nbColors);
    if (colorIndex == 0)
      colorIndex = nbColors;
    elseif (colorIndex < 0)
      colorIndex = 1;
    end
    color = colorOrder(colorIndex, :);
    if (colorIndex >= nbColors)
      colorIndex = colorIndex + 1;
      colorIndex = mod (colorIndex, nbColors);
      if (colorIndex == 0)
        colorIndex = 1;
      end
    else
      colorIndex = colorIndex + 1;
    end
    g = stemInternal(ax, X(:, k), Y(:,k), color, filled, propertiesLineA, propertiesLineB);
    G = [G, g];
  end
  if (nargout == 1)
    varargout{1} = G;
  end
end
%=============================================================================
function group = stemInternal(ax, X, Y, color, filled, propertiesLineA, propertiesLineB)
  axis(ax, 'auto');
  group = hggroup('Parent', ax);
  group.Visible = 'off';
  
  if isempty(propertiesLineA.Color) 
    propertiesLineA.Color = color;
  end
  if isempty(propertiesLineA.MarkerEdgeColor)
    propertiesLineA.MarkerEdgeColor = color;
  end
  
  if ~isfield(propertiesLineA, 'MarkerFaceColor')
    if filled
      propertiesLineA.MarkerFaceColor = propertiesLineA.Color;
    else
      propertiesLineA.MarkerFaceColor = 'none';
    end
  end 
  
  if isempty(propertiesLineB.Color) 
    propertiesLineB.Color = color;
  end
  hasVisibleA = isfield(propertiesLineA, 'Visible');
  hasVisibleB = isfield(propertiesLineB, 'Visible');
  if (hasVisibleA)
    isVisibleA = propertiesLineA.Visible;
  end
  if (hasVisibleB)
    isVisibleB = propertiesLineB.Visible;
  end

  propertiesLineA = reshape([fieldnames(propertiesLineA)'; struct2cell(propertiesLineA)'], 1, []);
  propertiesLineB = reshape([fieldnames(propertiesLineB)'; struct2cell(propertiesLineB)'], 1, []);
  H = plot(group, X, Y, 'Visible', 'off', propertiesLineA{:});
  h = {};
  for i = 1:length(X)
    h = [h, plot(group, [X(i) X(i)], [0 Y(i)],  'Visible', 'off', propertiesLineB{:})];
  end
  for i = 1:length(h)
    if hasVisibleB
      h{i}.Visible =  isVisibleB;
    else
      h{i}.Visible = 'on';
    end
  end
  if hasVisibleA
    H.Visible = isVisibleA;
  else
    H.Visible = 'on';
  end
  group.Visible = 'on';
end
%====================================================d=========================
function [parent, X, Y, filled, propertiesLineA, propertiesLineB] = parseArguments(inputArguments)
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
  isString = @(x) ischar(x) || isStringScalar(x);
  firstString = find (cellfun(isString, inputArguments), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  if ((firstString == 2) || (firstString == 3))
    if (firstString == 2)
      Y = inputArguments{1};
      X = linspace(0, size(Y, 2) - 1, size(Y, 2));
      inputArguments = inputArguments(2:end);
    else
      X = inputArguments{1};
      Y = inputArguments{2};
      inputArguments = inputArguments(3:end);
    end
  else
    error(_('Not enough input arguments.'));
  end
  
  [linespec, colorspec, markerspec, msg] = colstyle ('-o', 'plot');
  
  filled = false;
  isFilled = @(x) (ischar(x) || isStringScalar(x)) && strcmp(convertStringsToChars(x), 'filled');
  filledDectected = find (cellfun(isFilled, inputArguments));
  if (~isempty(filledDectected))
    filled = true;
    inputArguments(filledDectected) = [];
    nbInputArguments = length(inputArguments);
  end
  nbInputArguments = length(inputArguments);
  if (nbInputArguments > 0)
    if ~isValidGraphicsProperty('line', inputArguments{1})
      param = convertStringsToChars(inputArguments{1});
      if strcmp(param, 'filled')
        filled = true;
      else
        [linespec, colorspec, markerspec, msg] = colstyle (param, 'plot');
        if ~isempty(msg)
          error(msg);
        end
      end
      inputArguments = inputArguments(2:end);
      nbInputArguments = length(inputArguments);
    end
  end
  
  if min(size(X)) == 1, X = X(:); end
  if min(size(Y)) == 1, Y = Y(:); end
  
  propertiesLineA = struct();
  propertiesLineA.Color = colorspec;
  propertiesLineA.MarkerEdgeColor = colorspec;
  propertiesLineA.Marker = markerspec;
  propertiesLineA.MarkerSize = 6;
  
  for k = 1:2:nbInputArguments
    propertiesLineA.(inputArguments{k}) = inputArguments{k + 1};
  end
  propertiesLineA.LineStyle = 'none';

  
  propertiesLineB = struct();
  propertiesLineB.Color = colorspec;
  propertiesLineB.LineStyle = linespec;
  propertiesLineB.LineWidth = 0.5;
  for k = 1:2:nbInputArguments
    propertiesLineB.(inputArguments{k}) = inputArguments{k + 1};
  end
end
%====================================================d=========================
