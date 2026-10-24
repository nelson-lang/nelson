%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = tableMergeSetRows(A, B, ia, ib, partCodes, codesC)
  % Rows A(ia, :) then B(ib, :), put in the row order of the numeric result
  % codesC (sorted or stable) that has the same rows as partCodes.
  [~, order] = ismember(codesC, partCodes, 'rows');
  C = [A(ia, :); B(ib, :)];
  C = C(order, :);
end
%=============================================================================
