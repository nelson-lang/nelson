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
% the cell position of a reused axes must match a natively created subplot cell
f = figure('Visible', 'off');
refAx = subplot(2, 2, 3);
refPos = refAx.Position;
close(f);
%=============================================================================
% subplot(m, n, p, ax) converts an existing axes into the cell, it does not duplicate
f = figure('Visible', 'off');
a1 = axes();
a2 = subplot(2, 2, 3, a1);
assert_isequal(a1, a2);
assert_isequal(numel(findobj(f, '-depth', 1, 'Type', 'axes')), 1);
assert_isapprox(a1.Position, refPos, 1e-12);
close(f);
%=============================================================================
% subplot(m, n, p, pax) reuses a polaraxes without creating a cartesian axes
f = figure('Visible', 'off');
refPax = subplot(2, 3, 1);
refPaxPos = refPax.Position;
close(f);
f = figure('Visible', 'off');
pax = polaraxes();
ax = subplot(2, 3, 1, pax);
assert_isequal(ax, pax);
assert_isequal(ax.Type, 'polaraxes');
assert_isequal(numel(findobj(f, '-depth', 1, 'Type', 'polaraxes')), 1);
assert_isequal(numel(findobj(f, '-depth', 1, 'Type', 'axes')), 0);
assert_isapprox(pax.Position, refPaxPos, 1e-12);
close(f);
%=============================================================================
