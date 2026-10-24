%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function H = hankel(c, r)
  narginchk(1, 2);
  nargoutchk(0, 1);
  firstColumn = c(:);
  rowCount = length(firstColumn);
  if nargin < 2
    lastRow = zeros(size(firstColumn), 'like', firstColumn);
  else
    lastRow = r;
  end
  hasConflict = nargin == 2 && ~isempty(firstColumn) && ~isempty(lastRow) ...
    && firstColumn(rowCount) ~= lastRow(1);
  if hasConflict
    warning('Nelson:hankel:AntiDiagonalConflict', 'Last element of input column does not match first element of input row.');
  end
  lastRow = lastRow(:);
  columnCount = length(lastRow);
  values = [firstColumn; lastRow(2:columnCount)];
  H = values((1:rowCount).' + (0:columnCount - 1));
end
%=============================================================================
