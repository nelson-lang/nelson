%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
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
  
  Ts = plus_timesample(sys1.Ts, sys2.Ts);
  sys = ss(A, B, C, D);
  
  UserData = [];
  if ~isempty(sysA.UserData) && ~isempty(sysB.UserData)
    UserData = sysB.UserData;
  else
    if ~isempty(sysA.UserData)
      UserData = sysA.UserData;
    end
    if ~isempty(sysA.UserData)
      UserData = sysA.UserData;
    end
  end
  if ~isempty(UserData)
    sys.UserData = UserData;
  end
  
end
%=============================================================================
function Ts = plus_timesample(TsA, TsB)
  Ts = -1;
  if (TsA == -1) && (TsB ~= -1)
    Ts = TsB;
    return
  end
  if (TsB == -1) && (TsA ~= -1)
    Ts = TsA;
    return
  end
  if (TsB == TsA)
    Ts = TsA;
    return
  end
  if (TsA == -2)
    Ts = TsB;
    return
  end
  if (TsB == -2)
    Ts = TsA;
    return
  end
  error(_('Sampling times must agree.'));
end
%=============================================================================
