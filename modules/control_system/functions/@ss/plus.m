%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = plus(sys1, sys2)
  sysA = ss(sys1);
  sysB = ss(sys2);

  % basic implementation could be replaced with slicot function.
  A = blkdiag(sysA.A, sysB.A);
  B = [sysA.B; sysB.B];
  C = [sysA.C, sysB.C];
  D = sysA.D + sysB.D;

  Ts = lti.resolveSampleTime(sysA, sysB);
  sys = ss(A, B, C, D, Ts);
  sys = applyCommonMetadata(sys, sysA, sysB);

  userData = mergeUserData(sysA, sysB);
  if ~isempty(userData)
    sys.UserData = userData;
  end

end
