%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = padecoef(varargin)
  narginchk(1, 2)
  nargoutchk(0, 2)
  % see http://en.wikipedia.org/wiki/Pad%C3%A9_approximant
  
  if nargin == 1
    T = varargin{1};
    n = 1;
  else 
    T = varargin{1};
    n = round(varargin{2});
  end
  
  mustBeNumeric(T, 1);
  mustBeNumeric(n, 2);
  mustBeScalarOrEmpty(T, 1);
  mustBeScalarOrEmpty(n, 2);
  mustBeNonempty(T, 1);
  mustBeNonempty(n, 2);
  mustBePositive(T, 1);
  mustBePositive(n, 2);
  
  if (T == 0)
    varargout{1} = ones(size(T), 'like', T);
    if nargout == 2
      varargout{2} = varargout{1};
    end
    return
  end
  
  numerator = zeros(1, n + 1);
  numerator(n + 1) = 1;
  denominator = zeros(1, n + 1); 
  denominator(n + 1) = 1;
  
  for l = 1: n
    g = T * (n - l + 1) / ( 2 * n - l + 1) / l;
    idxIn = n + 2 - l;
    idxOut = n + 1 - l;
    numerator(idxOut) = (-g) * numerator(idxIn);
    denominator(idxOut) = g * denominator(idxIn);
  end
  
  varargout{1} = numerator / denominator(1);
  if (nargout == 2)
    varargout{2} = denominator / denominator(1);
  end
end
%=============================================================================
