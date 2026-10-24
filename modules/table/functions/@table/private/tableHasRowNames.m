%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = tableHasRowNames(rowNames, nRows)
  % A table without rows keeps the marker of its row names in a 0-by-1
  % RowNames; on a table with rows only a nonempty RowNames counts.
  tf = ~isempty(rowNames) || (nRows == 0 && iscolumn(rowNames));
end
%=============================================================================
