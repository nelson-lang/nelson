%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
delay = @(t, y) t - 0.05 + 0 * y(1);
sol = ddesd(@(t, y, z) - y + z, delay, 1, [0 0.2], ddeset('MaxStep', 0.02));
asserts.isequal(sol.solver, 'ddesd');
asserts.istrue(abs(sol.y(1, length(sol.x)) - 1) < 1e-8);
%=============================================================================
