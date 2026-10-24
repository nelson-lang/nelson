%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = tablePropagatesFunctionError(functionName)
  % Table math functions whose errors reach the caller as they are.
  tf = any(strcmp(functionName, {'realpow', 'reallog', 'realsqrt', 'nthroot'}));
end
%=============================================================================
