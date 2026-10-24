%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(obj)
  if width(obj) == 0
    return
  end
  numRows = height(obj);
  NB_LINES_TO_DISPLAY = 20;
  if numRows > NB_LINES_TO_DISPLAY
    table.dispTableCompact(obj);
  else
    table.dispTableFull(obj);
  end
end
%=============================================================================
