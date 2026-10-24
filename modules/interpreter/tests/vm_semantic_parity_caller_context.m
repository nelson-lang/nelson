%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function values = vm_semantic_parity_caller_context()
  localValue = 10;
  eval('localValue = localValue + 5;');
  vm_semantic_parity_assign_to_caller(7);
  callerValue = vm_semantic_parity_evalin_caller();
  clear assignedByCallee;
  values = [localValue, callerValue, exist('assignedByCallee', 'var')];
end
%=============================================================================
