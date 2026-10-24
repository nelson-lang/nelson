%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = filter(varargin)
  narginchk(3, 5);
  nargoutchk(0, 2);
  b = varargin{1};
  a = varargin{2};
  x = varargin{3};
  zi = [];
  dim = [];
  if nargin >= 4
    zi = varargin{4};
  end
  if nargin == 5
    dim = varargin{5};
  end
  validateCoefficients(b, a);
  if isempty(dim)
    dim = firstNonSingletonDimension(x);
  else
    validateDimension(dim);
  end
  if issparse(b) || issparse(a) || issparse(x) || issparse(zi)
    error('Nelson:elementary_functions:sparseInputsSupported', 'Sparse inputs are not supported.');
  end
  b = b(:).';
  a = a(:).';
  b = b ./ a(1);
  a = a ./ a(1);
  filterOrder = max(length(a), length(b)) - 1;
  b = [b, zeros(1, filterOrder + 1 - length(b))];
  a = [a, zeros(1, filterOrder + 1 - length(a))];
  if nargout > 1
    [y, zf] = builtin('__filter__', b, a, x, zi, dim);
    varargout{1} = y;
    varargout{2} = zf;
  else
    y = builtin('__filter__', b, a, x, zi, dim);
    varargout{1} = y;
  end
end
%=============================================================================
function validateCoefficients(b, a)
  if isempty(a) || isempty(b)
    error('Nelson:elementary_functions:filterCoefficientsEmpty', 'Filter coefficients must not be empty.');
  end
  if ~isvector(a) || ~isvector(b)
    error('Nelson:elementary_functions:filterCoefficientsVectors', 'Filter coefficients must be vectors.');
  end
  if a(1) == 0
    error('Nelson:elementary_functions:firstDenominatorFilterCoefficientNon', 'First denominator filter coefficient must be nonzero.');
  end
end
%=============================================================================
function validateDimension(dim)
  mustBeNumeric(dim);
  mustBeScalarOrEmpty(dim);
  mustBeFinite(dim);
  mustBeInteger(dim);
  mustBePositive(dim);
end
%=============================================================================
function dim = firstNonSingletonDimension(x)
  shape = size(x);
  dim = find(shape ~= 1, 1, 'first');
  if isempty(dim)
    dim = 1;
  end
end
%=============================================================================
