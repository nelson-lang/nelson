%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = shiftdim(varargin)
  % This function, shiftdim, is designed to shift the dimensions of an input array. 
  % It can handle one or two input arguments and returns one or two outputs accordingly.
  narginchk(1, 2);
  nargoutchk(0, 2);
  x = varargin{1};
  if (nargin == 1)
    [b, nshifts] = shiftdimOneInput(x);
  else
    n = varargin{2};
    [b, nshifts] = shiftdimTwoInputs(x, n);
  end
  
  varargout{1} = b;
  if nargout > 1
    varargout{2} = nshifts;
  end
end
%=============================================================================
function [b, nshifts] = shiftdimOneInput(x)
  if isrow(x) && ~isscalar(x)
    nshifts = 1;
    b = x.';
    return;
  end
  
  if ismatrix(x)
    nshifts = 0;
    b = x;
    return;
  end
  
  sz = size(x);
  n = find(sz ~= 1, 1) - 1;
  nshifts = n;
  if isequal(n, 0) || isempty(n)
    b = x;
    nshifts = 0;
    return
  end
  
  if (n > 0) % If the nonsingleton dimension is greater than zero.
    if n == 1 && ismatrix(x) % If the nonsingleton dimension is 1 and x is a matrix.
      b = x.'; % Transpose the matrix.
    else
      
      b = permute(x, [n + 1:ndims(x),1:n]); % Permute the dimensions of x.
    end
    return
  end
  b = reshape(x, [ones(1, -n), sz]); % Reshape x with singleton dimensions removed.
end
%=============================================================================
function [b, nshifts] = shiftdimTwoInputs(x, n)
  if n > 0
    n = rem(n, ndims(x));
  end
  sz = size(x);
  nshifts = n;
  if isequal(n,0) || isempty(n) % If the number of shifts is zero or empty.
    b = x; % Return x as it is.
    nshifts = 0;
  elseif (n > 0) % If the number of shifts is greater than zero.
    if n == 1 && ismatrix(x) % If the number of shifts is 1 and x is a matrix.
      b = x.'; % Transpose the matrix.
    else
      b = permute(x, [n+1:ndims(x), 1:n]); % Permute the dimensions of x.
    end
  else
    b = reshape(x, [ones(1, -n), sz]); % Reshape x with singleton dimensions removed.
  end
end
%=============================================================================
