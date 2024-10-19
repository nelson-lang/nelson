%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = acosh(X)
  try
    A = feval('@table/unaryFunctionTableHelper', X, @acosh);
  catch Exception
    throwAsCaller(Exception);
  end
end
%=============================================================================
