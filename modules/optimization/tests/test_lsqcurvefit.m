%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
xdata = (0:5).';
ydata = 2 * exp(0.25 * xdata);
opts = optimoptions('lsqnonlin', 'Display', 'off', 'StepTolerance', 1e-10);
[x, resnorm, residual, exitflag] = lsqcurvefit(@(p, x) p(1) * exp(p(2) * x), [1; 0.1], xdata, ydata, [], [], opts);
asserts.isapprox(x, [2; 0.25], 1e-6);
asserts.istrue(resnorm < 1e-10);
asserts.istrue(norm(residual) < 1e-5);
asserts.istrue(exitflag > 0);
%=============================================================================
