%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
reverseStr = '';
for idx = 1 : 100
  percentDone = idx;
  msg = sprintf('Percent done: %3.1f', percentDone);
  fprintf([reverseStr, msg]);
  reverseStr = repmat(sprintf('\b'), 1, length(msg));
end
%=============================================================================

