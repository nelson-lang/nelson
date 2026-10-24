%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tableCheckRowIndex(rows, nRows)
  % A row subscript that reads beyond the height of the table.
  if ~isempty(rows) && max(rows) > nRows
    error('Nelson:table:rowIndexOutOfRange', _('Row index exceeds table dimensions.'));
  end
end
%=============================================================================
