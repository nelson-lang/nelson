%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ltiCheckSampleTimeCompatibility(left, right)
  if isstatic(left) || isstatic(right) || left.Ts == right.Ts
    return
  end
  if left.Ts == -1 || right.Ts == -1
    return
  end
  error('Nelson:control_system:sampleTimeMismatch', 'Sampling times must agree.');
end
%=============================================================================
