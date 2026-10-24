%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A fill() polygon is a flat (single-plane) patch: the web figure must report
% it as 2D so the toolbar does not offer Rotate/View and keeps the same height
% as an ordinary 2D plot. Only a genuine 3D patch whose vertices span more than
% one z plane (fill3 with varying z) stays 3D and rotatable. The web is3d flag
% must agree with is2D(axes), which the figure interaction macro relies on.
%=============================================================================
f = figure('Visible', 'off');
fill([0, 1, 1, 0], [0, 0, 1, 1], 'r');
document = jsondecode(__web_display_list__(f));
assert_isequal(document.is3d, false);
assert_isequal(is2D(gca()), true);
close(f);
%=============================================================================
% A flat patch drawn directly (no explicit z) is 2D as well.
f = figure('Visible', 'off');
patch([0, 1, 1, 0], [0, 0, 1, 1], 'r');
document = jsondecode(__web_display_list__(f));
assert_isequal(document.is3d, false);
assert_isequal(is2D(gca()), true);
close(f);
%=============================================================================
% A genuine 3D fill3 polygon (z varies) stays 3D.
f = figure('Visible', 'off');
fill3([0, 1, 1, 0], [0, 0, 1, 1], [0, 0, 1, 2], 'r');
document = jsondecode(__web_display_list__(f));
assert_isequal(document.is3d, true);
assert_isequal(is2D(gca()), false);
close(f);
%=============================================================================
