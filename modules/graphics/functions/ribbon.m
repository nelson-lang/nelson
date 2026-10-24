%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ribbon(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  nbInputArguments = length(inputArguments);
  if (nbInputArguments < 1)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if (isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'axes'))
    designatedParent = inputArguments{1}(1);
    inputArguments = inputArguments(2:end);
  else
    % The pair is pulled out because the surfaces are given a parent of their
    % own below: left in place it would reach surface a second time.
    [designatedParent, inputArguments] = graphicsExtractParentProperty(inputArguments);
  end
  if isgraphics(designatedParent, 'hggroup')
    % The ribbons are drawn in an axes, so a group designated as target
    % resolves to the axes holding it, and that axes is the one to start anew.
    designatedParent = ancestor(designatedParent, 'axes');
  end
  go = newplotTarget(designatedParent);
  nbInputArguments = length(inputArguments);

  % Split the data from the name/value pairs: without this the first property
  % name is taken for the ribbon width and the call fails on the data check.
  firstProperty = find(cellfun('isclass', inputArguments, 'char'), 1);
  if (isempty(firstProperty))
    firstProperty = nbInputArguments + 1;
  end
  propertiesList = inputArguments(firstProperty:end);
  inputArguments = inputArguments(1:firstProperty - 1);
  nbInputArguments = length(inputArguments);

  nextPlot = go.NextPlot;
  go.NextPlot = 'add';

  width = 0.75;
  if (nbInputArguments == 1)
    y = inputArguments{1};
    if (isvector(y))
      y = y(:);
    end
    x = 1:size(y, 1);
  elseif (nbInputArguments == 2)
    x = inputArguments{1};
    y = inputArguments{2};
  else
    x = inputArguments{1};
    y = inputArguments{2};
    width = inputArguments{3};
  end

  if (~isequal(size(x), size(y)))
    if (~isvector(x) || size(y, 1) ~= numel(x))
      error('Nelson:ribbon:invalidX', 'X must have the same dimensions as Y or be a vector with the same number of rows as Y.');
    end
    x = repmat(x(:), 1, size(y, 2));
  end

  h = [];
  for k = size(y, 2):-1:1
    X = [k - width / 2, k + width / 2];
    Y = x(:, k);
    Z = [y(:, k), y(:, k)];
    [X, Y] = meshgrid(X, Y);
    C = repmat(k, size(Z));
    h(k) = surface(go, X, Y, Z, C, propertiesList{:});
  end

  nbRibbons = size(y, 2);
  if (nbRibbons > 1)
    go.CLim = [1, nbRibbons];
  else
    go.CLim = [0, 2];
  end

  switch nextPlot
    case {'replacechildren'}
      view(go, 3);
    case {'replaceall', 'replace'}
      view(go, 3);
      grid(go, 'on');
  end
  go.NextPlot = nextPlot;

  varargout = {};
  if (nargout > 0)
    varargout{1} = h;
  end
end
%=============================================================================
