%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
assert_isequal(exist('knot'), 2);
%=============================================================================
[x, y, z] = knot();
% (m + 1) x (n + 1) with m = 20 facets around the tube and n = 60 sections.
assert_isequal(size(x), [21 61]);
assert_isequal(size(y), [21 61]);
assert_isequal(size(z), [21 61]);
%=============================================================================
% Deterministic reference points of the trefoil tube.
assert_isapprox(x(1, 1), -1.17562028988035, 1e-10);
assert_isapprox(y(1, 1), 0.093511267285183, 1e-10);
assert_isapprox(z(1, 1), 0.973085136621342, 1e-10);
assert_isapprox(x(5, 25), 0.285976752691677, 1e-10);
assert_isapprox(y(5, 25), 1.92002981103084, 1e-10);
assert_isapprox(z(5, 25), 2.29769475728095, 1e-10);
assert_isapprox(x(11, 31), -1.82856835300723, 1e-10);
assert_isapprox(y(11, 31), -2.04578157137253, 1e-10);
assert_isapprox(z(11, 31), 3.99263453146236, 1e-10);
%=============================================================================
% The tube is closed along the knot curve (first and last column match).
assert_isapprox(x(:, 1), x(:, end), 1e-10);
assert_isapprox(y(:, 1), y(:, end), 1e-10);
assert_isapprox(z(:, 1), z(:, end), 1e-10);
% The cross-section is closed (first and last row match).
assert_isapprox(x(1, :), x(end, :), 1e-10);
assert_isapprox(y(1, :), y(end, :), 1e-10);
assert_isapprox(z(1, :), z(end, :), 1e-10);
%=============================================================================
f = figure('Visible', 'off');
knot();
h = findobj('type', 'surface');
assert_isequal(numel(h), 1);
assert_isequal(size(get(h, 'XData')), [21 61]);
% The tube is colored along its length.
assert_isequal(size(get(h, 'CData')), [21 61]);
assert_istrue(isgraphics(f));
% The tube is drawn smooth (interpolated shading, no visible mesh edges) and lit.
assert_isequal(get(h, 'FaceColor'), 'interp');
assert_isequal(get(h, 'EdgeColor'), 'none');
assert_isequal(get(h, 'FaceLighting'), 'gouraud');
% The axes are hidden.
ax = get(h, 'Parent');
assert_isequal(char(get(ax, 'Visible')), 'off');
close(f);
%=============================================================================
% A target axes can be supplied.
f = figure('Visible', 'off');
ax = axes();
knot(ax);
h = findobj(ax, 'type', 'surface');
assert_isequal(numel(h), 1);
close(f);
%=============================================================================
