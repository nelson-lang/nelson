%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function x = diff (varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  
  if (nargin < 2)
    k = 1;
  else
    k = varargin{2};
    if isempty(k)
      k = 1;
    end
    isPositiveIntegerValue = isscalar(k) && (k > 0) && (k == round(k));
    if ~isPositiveIntegerValue
      error ('Nelson:diff:differenceOrderMustBePositiveInteger', _('Difference order N must be a positive integer scalar.'));
    end
  end
  x = varargin{1};
  sz = size(x);
  nd = ndims(x);
  if (nargin == 3)
    dim = varargin{3};
    isPositiveIntegerDimension = isscalar (dim) && (dim == round(dim)) && (dim > 0) && (dim < (nd + 1));
    if ~isPositiveIntegerDimension
      error ('Nelson:getdimarg:dimensionMustBePositiveInteger', _('Dimension argument must be a positive integer scalar within indexing range.'));
    end
  else
    dim = 1;
    while ((sz(dim) == 1) && (dim < nd + 1))
      dim = dim + 1;
    end
    if (dim > nd)
      dim = 1;
    end
  end
  
  if nargin < 3
    if (sum(sz - 1) < k)
      if issingle(x)
        x = single([]);
      else
        x = [];
      end;
    else
      idx1 = cell();
      for i = 1:nd
        idx1{i} = 1:sz(i);
      end
      idx2 = idx1;
      while (k)
        n = sz(dim);
        for i = 1 : min(k, n - 1)
          idx1{dim} = 2 : (n - i + 1);	
          idx2{dim} = 1 : (n - i);	
          x = x(idx1{:}) - x(idx2{:});
        end
        idx1{dim} = 1;
        idx2{dim} = 1;
        k = k - min (k, n - 1);
        dim = dim + 1;
      end
    end
  else
    if (sz(dim) <= k)
      sz(dim) = 0;
      if issingle(x)
        x = zeros(sz, 'single');
      else
        x = zeros(sz);
      end
    else
      n = sz(dim);
      idx1 = cell();
      for i = 1:nd
        idx1{i} = 1:sz(i);
      end
      idx2 = idx1;
      for i = 1 : k;
        idx1{dim} = 2 : (n - i + 1);	
        idx2{dim} = 1 : (n - i);	
        x = x(idx1{:}) - x(idx2{:});
      end
    end
  end
end
