%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [values, ia, ic] = tableUniqueStable(valuesIn)
  % Stable unique of the row key strings. The builtin 'unique' on a cellstr
  % runs in O(n log n), replacing the former O(n*G) seen-loop.
  [values, ia, ic] = unique(valuesIn, 'stable');
  values = values(:);
  ia = ia(:);
  ic = reshape(ic, size(valuesIn));
end
%=============================================================================
