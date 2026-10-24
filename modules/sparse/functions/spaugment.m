%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function S = spaugment(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);

  A = varargin{1};
  if ((~isnumeric(A) && ~islogical(A)) || ndims(A) ~= 2)
    error('Nelson:types:numericOrLogical2DExpected', 'A numeric or logical 2-D matrix expected.');
  end
  if (isempty(A))
    error('Nelson:spaugment:emptyMatrix', 'A must be a non-empty matrix.');
  end

  if (nargin == 2)
    c = varargin{2};
    if ((~isnumeric(c) && ~islogical(c) && ~ischar(c)) || isempty(c))
      error('Nelson:spaugment:invalidC', 'C must be a non-empty numeric, logical, or character array.');
    end
    if (ndims(c) ~= 2)
      error('Nelson:spaugment:cMustBe2D', 'C must be a 2-D array.');
    end
    c = double(c(1));
  else
    c = max(max(abs(A))) / 1000;
  end

  [m, n] = size(A);
  if (c == 0)
    C = sparse(m, m);
  else
    C = sparse(1:m, 1:m, c * ones(m, 1), m, m);
  end
  S = [C, sparse(A); sparse(A'), sparse(n, n)];
end
%=============================================================================
