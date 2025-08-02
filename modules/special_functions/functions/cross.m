%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = cross(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  
  a = varargin{1};
  b = varargin{2};
  
  dim = [];
  if nargin == 3
    dim = varargin{3};
    if ~isscalar(dim)
      error(_('Dimension argument must be a positive integer scalar.'));
    end
  end
  
  vectorsOnlyCase = isvector(a) && isvector(b);
  if ~vectorsOnlyCase
    c = generalCaseCrossProduct(a, b, dim, nargin);
  else 
    c = vectorCrossProduct(a, b, dim, nargin);
  end
end
%=============================================================================
function c = vectorCrossProduct(a, b, dim, nRhs)
  
  isWrongDimRhs3 = (nRhs == 3) && (size(a, dim)~=3 || size(b, dim)~=3);
  isWrongDimRhs2 = (nRhs == 2) && (length(a) ~= 3 || length(b) ~= 3);
  
  if isWrongDimRhs3 || isWrongDimRhs2
    error(_('Length must 3 in the dimension in which the cross product expected.'));
  end
  
  c1 = a(2) .* b(3) - a(3) .* b(2);
  c2 = a(3) .* b(1) - a(1) .* b(3);
  c3 = a(1) .* b(2) - a(2) .* b(1);
  
  allColumns = iscolumn(a) && iscolumn(b);
  if ~allColumns
    c = [c1, c2, c3];
  else
    c = [c1; c2; c3];
  end
end
%=============================================================================
function c = generalCaseCrossProduct(a, b, dim, nRhs)
  if ~isequal(size(a), size(b))
    error(_('Same size expected.'));
  end
  if (nRhs == 3 && size(a, dim) ~= 3)
    error(_('Length must 3 in the dimension in which the cross product expected.'));
  else
    if (nRhs == 2)
      dim = find(size(a) == 3, 1);
      if isempty(dim)
        error(_('At least one dimension of length 3 expected.'));
      end
    end
  end
  perm = [dim:max(length(size(a)), dim), 1:dim - 1];
  a = permute(a, perm);
  b = permute(b, perm);
  c = zeros(size(a));
  c([3, 1, 2], :) = a([1, 2, 3], :) .* b([2, 3, 1], :) - a([2, 3, 1], :) .* b([1, 2, 3], :);
  c = reshape(c, size(a));
  c = ipermute(c, perm);
end
%=============================================================================
