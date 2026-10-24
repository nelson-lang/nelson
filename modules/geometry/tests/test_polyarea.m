%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unit square
asserts.isapprox(polyarea([0 1 1 0], [0 0 1 1]), 1, 1e-12);
%=============================================================================
% triangle
asserts.isapprox(polyarea([0 1 0], [0 0 1]), 0.5, 1e-12);
%=============================================================================
% orientation does not matter (absolute area)
asserts.isapprox(polyarea([0 0 1 1], [0 1 1 0]), 1, 1e-12);
%=============================================================================
% column vectors
asserts.isapprox(polyarea([0; 1; 1; 0], [0; 0; 1; 1]), 1, 1e-12);
%=============================================================================
% per-column areas for a matrix
asserts.isapprox(polyarea([0 0; 1 1; 1 1; 0 0], [0 0; 0 0; 1 1; 1 1]), [1 1], 1e-12);
%=============================================================================
% explicit dimension
asserts.isapprox(polyarea([0 1 1 0]', [0 0 1 1]', 1), 1, 1e-12);
%=============================================================================
% regular unit-radius pentagon area = (5/2) sin(72deg)
t = linspace(0, 2 * pi, 6);
t(end) = [];
asserts.isapprox(polyarea(cos(t), sin(t)), 2.5 * sin(2 * pi / 5), 1e-12);
%=============================================================================
% size mismatch errors
cmd = 'polyarea([1 2 3], [1 2])';
asserts.checkerror(cmd, _('X and Y must have the same size.'));
%=============================================================================
