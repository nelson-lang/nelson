%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = binaryFunctionTableHelper(A, B, fun)
  if istable(A) && istable(B)
    try
      R = funTableTable(A, B, fun);
    catch ME
      throwAsCaller(ME);
    end
  else
    try
      R = funArrayTable(A, B, fun);
    catch ME
      throwAsCaller(ME);
    end
  end
end
%=============================================================================
function R = funTableTable(A, B, fun)
  R = A;
  if ~isequal(A.Row, B.Row)
    error(_('Tables must have the same names for rows.'));
  end
  try
    for k = 1:size(R, 2)
      R{:, k} = fun(A{:, k}, B{:, k});
      R.Properties.VariableTypes(k) = class(R{:, k});      
    end
  catch
    functionName = func2str(fun);
    className = class(R{:, k});
    msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), functionName, className);
    ME = MException('Nelson:table:math:FunFailed', msg);
    throwAsCaller(ME);
  end
end
%=============================================================================
function R = funArrayTable(A, B, fun)
  if istable(A)
    R = A;
    X = B;
    tableFirst = true;
  else
    R = B;
    X = A;
    tableFirst = false;
  end
  try  
    for k = 1:size(R, 2)
      if tableFirst
        R{:, k}  = fun(A{:, k}, X); 
      else
        R{:, k}  = fun(X, B{:, k});
      end
      R.Properties.VariableTypes(k) = class(R{:, k});
    end
  catch
    functionName = func2str(fun);
    className = class(R{:, k});
    msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), functionName, className);
    ME = MException('Nelson:table:math:FunFailed', msg);
    throwAsCaller(ME);
  end
end
%=============================================================================
