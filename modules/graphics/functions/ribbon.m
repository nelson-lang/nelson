%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ribbon (varargin)
  nargoutchk(0, 1);
  inputArguments = varargin;
  nbInputArguments = nargin;
  if (nbInputArguments < 1)
    error(_('Wrong number of input arguments.'));
  end
  if (isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'axes'))
    go = inputArguments{1}(1);
    inputArguments = inputArguments(2:end);
    nbInputArguments = length(inputArguments);
  else
    go = newplot();
  end
  
  nextPlot = go.NextPlot;
  go.NextPlot = 'add';
  
  width = 0.75;
  if (nbInputArguments == 1)
    y = inputArguments{1};
    if (isvector (y))
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
    if (~isvector(x) || size(y, 1) ~= numel (x))
      error (_('X must have the same dimensions as Y or be a vector with the same number of rows as Y.'));
    end
    x = repmat (x(:), 1, size(y, 2));
  end
  
  h = [];
  for k = size (y, 2):-1:1
    X = [k - width / 2, k + width / 2];
    Y = x(:, k);
    Z = [y(:, k), y(:, k)];
    [X, Y] = meshgrid (X, Y);
    C = repmat (k, size(Z));
    h(k) = surface (go, X, Y, Z, C);
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
