%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
solvers = {@ode113, @ode78, @ode89, @ode15s, @ode23s, @ode23t, @ode23tb};
options = odeset('RelTol', 1e-3, 'AbsTol', 1e-6, ...
  'InitialStep', 0.05, 'MaxStep', 0.1, 'Jacobian', -1);

for k = 1:length(solvers)
  solver = solvers{k};
  sol = solver(@(t, y) - y, [0.2 0.1 0], exp(-0.2), options);
  asserts.isequal(size(sol.x, 1), 1);
  asserts.isequal(size(sol.y, 1), 1);
  asserts.istrue(abs(sol.x(1) - 0.2) < 1e-12);
  asserts.istrue(abs(sol.x(length(sol.x))) < 1e-12);
  asserts.istrue(max(abs(sol.y(:) - exp(-sol.x(:)))) < 2e-2);
  ymiddle = deval(sol, 0.05);
  asserts.istrue(abs(ymiddle - exp(-0.05)) < 2e-2);
  asserts.isequal(sol.solver, func2str(solver));
end

implicitOptions = odeset('RelTol', 1e-4, 'AbsTol', 1e-7, ...
  'InitialStep', 0.05, 'MaxStep', 0.1);
[ti, yi] = ode15i(@(t, y, yp) yp + y, [0.2 0.1 0], exp(-0.2), -exp(-0.2), implicitOptions);
asserts.isequal(ti, [0.2; 0.1; 0]);
asserts.istrue(max(abs(yi - exp(-ti))) < 5e-3);
implicitSol = ode15i(@(t, y, yp) yp + y, [0 0.2], 1, -1, implicitOptions);
asserts.istrue(abs(deval(implicitSol, 0.1) - exp(-0.1)) < 5e-3);
%=============================================================================
