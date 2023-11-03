%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bar(varargin)
  % basic implementation (stacked, group not yet implemented)
  % bar(y)
  % bar(x, y)
  % bar(..., width)
  % bar(..., color)
  % bar(ax, ...)
  % b = bar(...)
  % b = bar(ax, x, y, width, color, ...)
  narginchk(1, 100);
  nargoutchk(0, 1);
  [parent, X, Y, width, color, otherProperties] = parseArguments(varargin);
  h = barPlot(parent, X, Y, width, color, otherProperties);
  if (nargout == 1)
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, X, Y, width, color, otherProperties] = parseArguments(inputArguments)
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
  defaultWidth = 0.8;
  defaultColor = getColorAndUpdateIndex(parent);
  
  supportedColorString = string(getColorShortNameList());
  isString = @(x) (ischar(x) || isStringScalar(x)) && ~matches(convertCharsToStrings(x), supportedColorString);
  firstString = find (cellfun(isString, inputArguments), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  
  if (firstString > nbInputArguments)
    otherProperties = struct([]);
  else
    otherProperties = inputArguments(firstString:end);
    otherProperties = struct(otherProperties{:});
    inputArguments = inputArguments(1:firstString-1);
  end
  nbInputArguments = length(inputArguments);
  
  if nbInputArguments == 1
    [X, Y, width, color] = parseOneArgument(inputArguments, defaultWidth, defaultColor);
  end
  
  if nbInputArguments == 2
    [X, Y, width, color] = parseTwoArguments(inputArguments, defaultWidth, defaultColor);
  end
  
  if nbInputArguments == 3
    [X, Y, width, color] = parseThreeArguments(inputArguments, defaultWidth, defaultColor);
  end
  if min(size(X)) == 1, X = X(:); end
  if min(size(Y)) == 1, Y = Y(:); end
end
%=============================================================================
function [X, Y, width, color] = parseThreeArguments(inputArguments, defaultWidth, defaultColor)
  width = defaultWidth;
  color = defaultColor;
  X = inputArguments{1};
  X = X(:)';
  if isscalar(inputArguments{2})
    if isnumeric(inputArguments{2})
      width = inputArguments{2};
    elseif ischar(inputArguments{2}) || isStringScalar(inputArguments{2})
      color = convertStringsToChars(inputArguments{2});
    else
      error(_('Unrecognized option for second argument.'));
    end
  else
    Y = inputArguments{2};
    Y = Y(:)';
  end
  if isnumeric(inputArguments{3})
    width = inputArguments{3};
  elseif ischar(inputArguments{3}) || isStringScalar(inputArguments{3})
    color = convertStringsToChars(inputArguments{3});
  else
    error(_('Unrecognized option for third argument.'));
  end
end
%=============================================================================
function [X, Y, width, color] = parseTwoArguments(inputArguments, defaultWidth, defaultColor)
  width = defaultWidth;
  color = defaultColor;
  if isscalar(inputArguments{2})
    if isnumeric(inputArguments{2})
      width = inputArguments{2};
    elseif ischar(inputArguments{2}) || isStringScalar(inputArguments{2})
      color = convertStringsToChars(inputArguments{2})
    else
      error(_('Unrecognized option for second argument.'));
    end
    Y = inputArguments{1};
    Y = Y(:)';
    X = linspace(0, size(Y, 2) - 1, size(Y, 2)) + 1;
  else
    X = inputArguments{1};
    X = X(:)';
    Y = inputArguments{2};
    Y = Y(:)';
  end
end
%=============================================================================
function [X, Y, width, color] = parseOneArgument(inputArguments, defaultWidth, defaultColor)
  width = defaultWidth;
  color = defaultColor;
  Y = inputArguments{1};
  Y = Y(:)';
  X = linspace(0, size(Y, 2) - 1, size(Y, 2)) + 1;
end
%=============================================================================
function h = barPlot(parent, X, Y,width, color, otherProperties)
  needUpdateXLabel = false;
  if isstring(X)
    needUpdateXLabel = true;
    xLabels = X;
    X = 1:length(X);
    X = reshape(X, size(xLabels))   
  end
  df = width * diff(X) / 2;
  df(end + 1) = df(end);
  x_r = X(1:end) - df;
  x_l = X(1:end) + df;
  
  n = length(X);
  onesXr = ones(size(x_r));
  vertices = [x_r, zeros(size(x_r)), onesXr; ...
  x_r, Y, onesXr; ...
  x_l, Y, onesXr; ...
  x_l, zeros(size(x_l)), onesXr];
  
  ind = (1:n)';
  faces = [ind, ind + n, ind + 2 * n, ind + 3 * n];
  barProperties = struct('Parent', parent, 'Faces', faces, 'Vertices', vertices, 'FaceColor', color);
  for name = fieldnames(otherProperties)'
    barProperties.(name{1}) = otherProperties.(name{1});
  end
  barProperties = reshape([fieldnames(barProperties)'; struct2cell(barProperties)'], 1, []);
  h = patch(barProperties{:});
  if needUpdateXLabel
    ax = gca();
    ticks = ax.XTickLabel;
    res = cell(1, numel(ticks));
    for i = 1:numel(X)
      idx = find(strcmp(ticks, num2str(X(i))));
      if ~isempty(idx)
        res{idx} = xLabels(i);
      end
    end
    ax.XTickLabel = string(res);
  end
end
%=============================================================================
