%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
odeCheckSolverForTest(@ode78);
%=============================================================================
% High-order accuracy: the Verner 7(6) pair must track a tight tolerance
% (guards the Butcher tableau transcription).
sol = ode78(@(t, y)[y(2); -y(1)], [0 10], [1; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
asserts.istrue(max(abs(sol.y(:, end) - [cos(10); -sin(10)])) < 1e-8);
%=============================================================================

