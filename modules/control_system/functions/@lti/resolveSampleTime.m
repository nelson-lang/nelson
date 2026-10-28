%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Ts = resolveSampleTime(left, right)
  lti.checkSampleTimeCompatibility(left, right);
  if isstatic(left)
    Ts = right.Ts;
    return
  end
  if isstatic(right)
    Ts = left.Ts;
    return
  end
  if left.Ts == -1 && right.Ts ~= -1
    Ts = right.Ts;
    return
  end
  if right.Ts == -1 && left.Ts ~= -1
    Ts = left.Ts;
    return
  end
  Ts = left.Ts;
end
%=============================================================================
