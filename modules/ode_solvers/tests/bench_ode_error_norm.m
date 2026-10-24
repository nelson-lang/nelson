%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
n = 2000;
y = linspace(1, 2, n).';
yhigh = y + 1e-7 * sin((1:n).');
ylow = yhigh - 1e-8 * cos((1:n).');
absTol = 1e-8 * ones(n, 1);
relTol = 1e-6;
fprintf('bench_ode_error_norm\n');
tic();
for k = 1:500
  scale = absTol + max(abs(y), abs(yhigh)) * relTol;
  macroErr = max(abs(yhigh - ylow) ./ scale);
end
macroTime = toc();
fprintf('  macro: %.6f s, err=%g\n', macroTime, macroErr);
if exist('__ode_error_norm__') == 5
  tic();
  for k = 1:500
    nativeErr = __ode_error_norm__(y, yhigh, ylow, absTol, relTol, 0);
  end
  nativeTime = toc();
  fprintf('  native: %.6f s, err=%g\n', nativeTime, nativeErr);
  fprintf('  ratio macro/native = %.2f\n', macroTime / max(nativeTime, eps));
  asserts.istrue(abs(nativeErr - macroErr) < 10 * eps);
else
  fprintf('  native: not loaded\n');
end
%=============================================================================
