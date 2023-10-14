%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = mtimes(sys1, sys2)
    sysA = ss(sys1);
    sysB = ss(sys2);

    Ts = mtimes_timesample(sysA.Ts, sysB.Ts);
    if isempty(sysA.A) && isempty(sysA.B) && isempty(sysA.C) && isscalar(sysA.D)
        A = sysB.A;
        B = sysA.D * sysB.B; 
        C = sysB.C;
        D = sysA.D * sysB.D;
    else
        if isempty(sysB.A) && isempty(sysB.B) && isempty(sysB.C) && isscalar(sysB.D)
            A = sysA.A;
            B = sysB.D * sysA.B;
            C = sysA.C;
            D = sysA.D * sysB.D;
        else
            [pA, mA] = size(sysA);
            [pB, mB] = size(sysB);
            if (mA ~= pA)
                error(_('Matrix dimensions must agree.'));
            end
            A = [[sysA.A, sysA.B * sysB.C];[zeros(size(sysB.A, 1), size(sysA.A,2)), sysB.A]];
            B = [sysA.B*sysB.D;sysB.B];
            C = [sysA.C, sysA.D * sysB.C];
            D = sysA.D * sysB.D;
        end
    end
    sys = ss(A, B, C, D, Ts);
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
function Ts = mtimes_timesample(TsA, TsB)
    if (TsA == -1) && (TsB ~= -1)
        Ts = TsB;
    else
        if (TsB == -1) && (TsA ~= -1)
            Ts = TsA;
        else
            if (TsB == TsA)
                Ts = TsA;
            else
                error(_('Sampling times must agree.'));
            end
        end
    end 
end
%=============================================================================
