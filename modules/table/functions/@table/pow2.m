%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = pow2(varargin)
  try
    if nargin == 1
      R = feval('@table/unaryFunctionTableHelper', varargin{1}, @pow2);
    else
      R = feval('@table/binaryFunctionTableHelper', varargin{1}, varargin{2}, @pow2);
    end
    catch Exception
    throwAsCaller(Exception);
  end
end
%=============================================================================
