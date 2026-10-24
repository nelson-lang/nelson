%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = bytecode_global_vector_slice_transfer(repeats)
  global bytecodeGlobalVectorA bytecodeGlobalVectorIndex
  value = 0;
  for k = 1:repeats
    j = bytecodeGlobalVectorIndex;
    block = bytecodeGlobalVectorA(j:j + 23);
    bytecodeGlobalVectorA(j + 24:j + 47) = block + k;
    bytecodeGlobalVectorIndex = 1 + mod(j + 1, 16);
    value = value + bytecodeGlobalVectorA(j);
  end
end
%=============================================================================
