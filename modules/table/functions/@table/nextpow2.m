%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = nextpow2(X)
  try
    A = feval('@table/unaryFunctionTableHelper', X, @nextpow2);
  catch Exception
    throwAsCaller(Exception);
  end
end
%=============================================================================
