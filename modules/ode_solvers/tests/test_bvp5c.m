%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
solinit = bvpinit(linspace(0, 1, 5), [0; 1]);
sol = bvp5c(@(x, y)[y(2); 0], @(ya, yb)[ya(1); yb(1) - 1], solinit, ...
  bvpset('RelTol', 1e-5, 'NMax', 100));
asserts.isequal(sol.solver, 'bvp5c');
asserts.istrue(max(abs(sol.y(1, :) - sol.x)) < 1e-4);
%=============================================================================
