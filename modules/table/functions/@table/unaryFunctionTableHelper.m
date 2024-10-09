%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = unaryFunctionTableHelper(A, fun)
  R = A;
  try
    for k = 1:size(R, 2)
      R{:, k}  = fun(A{:, k}); 
    end
  catch
    functionName = func2str(fun);
    className = class(A{:, k});
    msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), functionName, className);
    ME = MException('Nelson:table:math:FunFailed', msg);
    throwAsCaller(ME);
  end
end
%=============================================================================
