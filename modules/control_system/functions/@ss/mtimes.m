%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = mtimes(sys1, sys2)
  sysA = ss(sys1);
  sysB = ss(sys2);

  Ts = lti.resolveSampleTime(sysA, sysB);
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
        error('Nelson:control_system:dimagree:matrixDimensionsAgree', 'Matrix dimensions must agree.');
      end
      A = [[sysA.A, sysA.B * sysB.C]; [zeros(size(sysB.A, 1), size(sysA.A, 2)), sysB.A]];
      B = [sysA.B * sysB.D; sysB.B];
      C = [sysA.C, sysA.D * sysB.C];
      D = sysA.D * sysB.D;
    end
  end

  sys = ss(A, B, C, D, Ts);
  sys.E = computesE(sysA.E, sysB.E, size(sysA.A, 1), size(sysB.A, 1));
  sys = applySeriesMetadata(sys, sysA, sysB);
  userData = mergeUserData(sysA, sysB);
  if ~isempty(userData)
    sys.UserData = userData;
  end
end
%=============================================================================
function E = computesE(E1, E2, n1, n2)
  if ~isempty(E1) || ~isempty(E2)
    if isempty(E1)
      E1 = eye(n1, 'like', E2);
    elseif isempty(E2)
      E2 = eye(n2, 'like', E1);
    end
    E = zeros(n1 + n2, n1 + n2);
    E(1:n1, 1:n1) = E1;
    E(n1 + 1:n1 + n2, n1 + 1:n1 + n2) = E2;
  else
    E = [];
  end
end
%=============================================================================
