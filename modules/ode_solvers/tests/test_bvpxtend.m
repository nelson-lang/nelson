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
sol = bvp4c(@(x, y)[y(2); 0], @(ya, yb)[ya(1); yb(1) - 1], solinit, ...
  bvpset('RelTol', 1e-5, 'NMax', 100));
extended = bvpxtend(sol, linspace(0, 1, 9));
asserts.isequal(length(extended.x), 9);
asserts.istrue(max(abs(extended.y(1, :) - extended.x)) < 1e-4);
asserts.checkerror('bvpxtend(sol, [-0.1 0.5])', ...
  _('Extended mesh outside the solution interval requires an initial guess.'));
%=============================================================================
