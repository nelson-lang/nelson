%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = inv(sys)
  A = sys.A;
  B = sys.B;
  C = sys.C;
  D = sys.D;
  E = sys.E;
  
  withE = ~isempty(sys.E) || (rcond(sys.D) < eps())
  
  if (~withE)
    res = sys;
    BD = B / D;
    res.A = A - BD * C;
    res.B = BD;
    res.C = -(D \ C);
    res.D = inv(D);
  else
    res = sys;
    m = size(B, 2);
    n = size(A, 1);
    res.A = [A, B; C, D];
    res.B = [zeros(n, m); -eye(m)];
    res.C = [zeros(m, n), eye(m)];
    res.D = zeros(m, m);
    if isempty(E)
      E = eye(n, n);
    end
    res.E = [E, zeros(n, m); zeros(m, n + m)];
  end
end
%=============================================================================
