%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bytecode_global_multi_writer(a, b)
  global bytecodeGlobalMultiA bytecodeGlobalMultiB
  bytecodeGlobalMultiA = a;
  bytecodeGlobalMultiB = b;
end
%=============================================================================
