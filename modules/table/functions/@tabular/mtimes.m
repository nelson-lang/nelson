%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = mtimes(A, B)
  % Element-wise operation on a table or timetable combined with a scalar.
  checkTabularScalarOperand(A, B, 'mtimes');
  R = times(A, B);
end
%=============================================================================
