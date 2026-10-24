%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
x = [0 1 0];
y = [0 0 1];
z = [0 1 0];
h = fill3(x, y, z, 'red');
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(h.FaceColor, [1 0 0]);
asserts.isequal(size(h.Vertices), [3 3]);
%=============================================================================
f = figure('Visible', 'off');
x1 = [0 1 0];
y1 = [0 0 1];
z1 = [0 0 1];
x2 = x1 + 2;
h = fill3(x1, y1, z1, 'r', x2, y1, z1, 'b', 'FaceAlpha', 0.5);
asserts.isequal(size(h), [1 2]);
asserts.istrue(isgraphics(h(1), 'patch'));
asserts.istrue(isgraphics(h(2), 'patch'));
asserts.isapprox(h(1).FaceAlpha, 0.5, 1e-12);
asserts.isapprox(h(2).FaceAlpha, 0.5, 1e-12);
%=============================================================================
f = figure('Visible', 'off');
X = [0 2; 0 2; 1 3];
Y = [0 0; 1 1; 0 0];
Z = [0 0; 0 0; 1 1];
h = fill3(X, Y, Z, [0 1 0], 'EdgeColor', 'none');
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).EdgeColor, 'none');
asserts.isequal(h(2).EdgeColor, 'none');
%=============================================================================
