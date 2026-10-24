%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = ismethod(H, methodName)
  if ~isscalar(H)
    error('Nelson:scalarExpected', 'Wrong size for argument #%d: scalar expected.', 1);
  end
  if (strcmp(class(H), 'MPI_Comm') == false)
    error('Nelson:types:wrongArgumentTypeExpected', 'Wrong type for argument #%d: %s expected.', 1, 'MPI_Comm');
  end
  if ~ischar(methodName)
    error('Nelson:types:wrongArgumentTypeExpected', 'Wrong type for argument #%d: %s expected.', 2, 'string');
  end
  r = false;
end
%=============================================================================
