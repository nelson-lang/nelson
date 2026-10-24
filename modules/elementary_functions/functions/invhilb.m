%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = invhilb(n, requestedClassName)
  % https://nhigham.com/2020/06/30/what-is-the-hilbert-matrix/
  narginchk(1, 2)
  nargoutchk(0, 1)
  if nargin < 2
    className = 'double';
  else
    className = requestedClassName;
  end
  if isStringScalar(className)
    className = convertStringsToChars(className);
  end
  if ~strcmp(className, 'double') && ~strcmp(className, 'single')
    error('Nelson:elementary_functions:notSupportedClass', '#%d argument must be ''%s'' or ''%s''.', 2, 'double', 'single');
  end
  h = zeros(n, className);
  for row = 1:n
    for col = row:n
      entry = inverseHilbertEntry(n, row, col);
      h(row, col) = entry;
      h(col, row) = entry;
    end
  end
end
%=============================================================================
function entry = inverseHilbertEntry(order, row, col)
  signValue = 1 - 2 * rem(row + col, 2);
  left = nchoosek(order + row - 1, order - col);
  right = nchoosek(order + col - 1, order - row);
  center = nchoosek(row + col - 2, row - 1);
  entry = signValue * (row + col - 1) * left * right * center * center;
end
%=============================================================================
