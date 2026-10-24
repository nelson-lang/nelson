%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = [0 0; 1 0; 1 1; 0 1];
f = scatteredInterpolant(p, p(:, 1) + p(:, 2), 'nearest');
asserts.isequal(evaluate(f, [0.9 0.9]), 2);
asserts.isequal(f(0.9, 0.9), 2);
%=============================================================================
% Linear interpolant queried with one array per dimension.
f = scatteredInterpolant(p, p(:, 1) + p(:, 2));
asserts.isequal(f(0.25, 0.25), 0.5);
%=============================================================================
% Collinear (degenerate) points warn and return empty instead of erroring.
lastwarn('');
pc = [0 0; 1 1; 2 2; 3 3];
fc = scatteredInterpolant(pc, [1; 2; 3; 4]);
msg = lastwarn();
asserts.isequal(msg, _('The underlying triangulation is empty - the points may be collinear.'));
asserts.istrue(isempty(fc(1.5, 1.5)));
%=============================================================================
