%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
t = 0:0.1:1;
asserts.isequal(size(chirp(t)), size(t));
asserts.isequal(size(sawtooth(t)), size(t));
asserts.isequal(size(square(t)), size(t));
asserts.isequal(rectpuls(0), 1);
asserts.isequal(tripuls(0), 1);
%=============================================================================
t = [0 pi / 2 pi 3 * pi / 2 2 * pi];
asserts.isapprox(sawtooth(t), [-1 -0.5 0 0.5 -1], 1e-12);
asserts.isapprox(sawtooth(t, 0.5), [-1 0 1 0 -1], 1e-12);
asserts.isequal(square(t), [1 1 -1 -1 1]);
asserts.isequal(square(t, 25), [1 -1 -1 -1 1]);
asserts.isequal(rectpuls([-0.5 0 0.5], 1), [1 1 0]);
asserts.isequal(tripuls([-0.5 0 0.5], 1), [0 1 0]);
asserts.isapprox(chirp([0 0.5 1], 0, 1, 1), [1 0.70710678 -1], 1e-8);
%=============================================================================
asserts.isapprox(chirp([0 NaN 1]), [1 NaN 1], 1e-12);
asserts.isapprox(sawtooth([0 NaN pi]), [-1 NaN 0], 1e-12);
asserts.isequal(square([0 NaN pi]), [1 1 -1]);
asserts.isequal(rectpuls([-Inf NaN Inf]), [0 0 0]);
asserts.isequal(tripuls([-Inf NaN Inf]), [0 0 0]);
%=============================================================================
asserts.checkerror('chirp([0 1], 0, 1, 1, ''logarithmic'')', _('Frequencies must be nonzero and have the same sign for logarithmic sweeps.'));
%=============================================================================
