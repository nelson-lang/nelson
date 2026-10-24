%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
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
ax = axes('Parent', f);
e = images.roi.Ellipse(ax, 'Center', [1, 2], 'SemiAxes', [3, 1], ...
  'InteractionsAllowed', 'none', 'Color', [1, 0, 0], 'FaceAlpha', 0.5);
assert_isequal(class(e), 'images.roi.Ellipse');
assert_isequal(e.Center, [1, 2]);
assert_isequal(e.SemiAxes, [3, 1]);
assert_isapprox(e.FaceAlpha, 0.5, 1e-12);
% a single filled patch is drawn on the axes
ch = get(ax, 'Children');
assert_isequal(numel(ch), 1);
assert_istrue(isgraphics(ch(1), 'patch'));
close(f);
%=============================================================================
% defaults to the current axes when none is given
f2 = figure('Visible', 'off');
axes();
e2 = images.roi.Ellipse('Center', [0, 0], 'SemiAxes', [2, 2]);
assert_isequal(e2.Center, [0, 0]);
close(f2);
%=============================================================================
