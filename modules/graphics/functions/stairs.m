%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stairs(varargin)
  nargoutchk(0, 2);
  if (nargout == 2)
    narginchk(1, 2);
    [xb, yb] = stairsImplementation(varargin{:});
    varargout{1} = xb;
    varargout{2} = yb;
  else
    narginchk(1, 20);
    h = stairsPlot(varargin{:});
    if (nargout == 1)
      varargout{1} = h;
    end
  end
end
%=============================================================================
function [XB, YB] = stairsImplementation(varargin)
  [X, Y] = prepareXY(varargin{:});
  
  [m, n] = size(Y);
  if ((m == 1) && (n > 1))
    X = X';
    Y = Y';     
    [m, n] = size(Y);
  end
  
  r = 2 * m - 1;
  c = n;
  
  XB = zeros(r, c);
  YB = XB;
  
  idxA = 1:2:2*m;
  idxB = 2:2:2*(m-1);
  XB(idxA, :) = X;
  XB(idxB, :) = X(2:end, :);
  
  idxA = 1:2:r;
  YB(idxA, :) = Y;
  YB(idxB, :) = Y(1:(end - 1), :);
end
%=============================================================================
function [X, Y] = prepareXY(varargin)
  if (nargin == 1)
    Y = varargin{1};
    if isvector(Y)
      X = 1:length(Y);
      X = reshape(X, size(Y));
    else
      [m, n] = size(Y);
      X = [1:m]';
      X = repmat(X, 1, n);
    end
  else
    Y = varargin{2};
    if ~ismatrix(Y) 
      error(_('Y must be a vector or matrix.'));
    end
    X = varargin{1};
    
    if (isvector(X) && isvector(Y))
      X = reshape(X, length(X), 1);
      Y = reshape(Y, length(Y), 1);
    elseif (isvector(X) && ~isvector(Y))
      if (isrow(X))
        error(_('Dimensions do not match.'));
      end
      [mx, nx] = size(X);
      [my, ny] = size(Y);
      if ((nx == 1) && (mx == my))
        X = repmat(X, 1, ny);
      end
    elseif (~isvector(X) && isvector(Y))
      [mx, nx] = size(X);
      [my, ny] = size(Y);         
      if ((ny == 1) && (mx == my))
        Y = repmat(Y, 1, nx);
      end
    end
    
    if (~isempty(find([size(X) == size(Y)]==0)))
      error(_('Dimensions do not match.'));
    end
  end
end
%=============================================================================
function h = stairsPlot(varargin)
  inputArguments = varargin;
  nbInputArguments = length(inputArguments);
  if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    parent = inputArguments{1};
    inputArguments = inputArguments(2:end);
    nbInputArguments = nbInputArguments - 1;
  else
    parent = newplot();
  end
  if nbInputArguments > 1 && isnumeric(inputArguments{1}) && isnumeric(inputArguments{2})
    X = inputArguments{1};
    Y = inputArguments{2};
    inputArguments = inputArguments(3:end);
    [XB, YB] = stairsImplementation(X, Y);
  else
    if isnumeric(inputArguments{1})
      Y = inputArguments{1};
      inputArguments = inputArguments(2:end);
      [XB, YB] = stairsImplementation(Y);
    else
      error(_('Wrong number of output arguments.'));
    end
  end
  h = plot(parent, XB, YB, inputArguments{:});
end
%=============================================================================
