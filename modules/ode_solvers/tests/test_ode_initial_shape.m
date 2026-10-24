%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[t, y] = ode45(@(t, y)[y(2); -y(1)], [0 0.5], [1 0]);
asserts.isequal(size(t, 2), 1);
asserts.isequal(size(y, 2), 2);
asserts.isequal(y(1, :), [1 0]);

[tc, yc] = ode45(@(t, y)[y(2); -y(1)], [0 0.5], [1; 0]);
asserts.isequal(size(tc, 2), 1);
asserts.isequal(size(yc, 2), 2);
asserts.isequal(yc(1, :), [1 0]);
%=============================================================================
