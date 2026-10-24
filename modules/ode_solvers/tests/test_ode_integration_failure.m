%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A solution that blows up must return the partial solution with a warning,
% not raise an error.
lastwarn('');
sol = ode45(@(t, y) y ^ 2, [0 2], 1);
[msg, ~] = lastwarn();
asserts.istrue(isstruct(sol) || isobject(sol));
asserts.istrue(sol.x(end) < 2);
asserts.istrue(~isempty(msg));
asserts.istrue(all(isfinite(sol.y(:, end))));
%=============================================================================
% A well-posed problem must integrate to the end without any warning.
lastwarn('');
good = ode45(@(t, y) - y, [0 1], 1);
[msg2, ~] = lastwarn();
asserts.istrue(abs(good.x(end) - 1) < 1e-9);
asserts.istrue(isempty(msg2));
asserts.istrue(abs(good.y(end) - exp(-1)) < 1e-3);
%=============================================================================
