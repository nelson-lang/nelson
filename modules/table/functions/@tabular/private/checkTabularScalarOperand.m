%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function checkTabularScalarOperand(A, B, functionName)
  if istabular(A)
    other = B;
  else
    other = A;
  end
  if istabular(other) || ~isscalar(other)
    error(['Nelson:table:', functionName, ':nonScalarOperand'], _('When one input is a table or timetable, the other input must be a scalar.'));
  end
end
%=============================================================================
