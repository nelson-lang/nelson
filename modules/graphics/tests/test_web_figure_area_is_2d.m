%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A flat patch (area/fill) lies in a single plane: the web figure must report
% it as 2D so the toolbar does not offer Rotate/View and the figure keeps the
% same height as an ordinary 2D plot.
%=============================================================================
t = linspace(0, 2 * pi, 100);
%=============================================================================
f = figure('Visible', 'off');
area(t, sin(t));
document = jsondecode(__web_display_list__(f));
assert_isequal(document.is3d, false);
close(f);
%=============================================================================
f = figure('Visible', 'off');
plot(t, sin(t));
document = jsondecode(__web_display_list__(f));
assert_isequal(document.is3d, false);
close(f);
%=============================================================================
% A genuine 3D surface stays 3D.
f = figure('Visible', 'off');
surf(peaks(20));
document = jsondecode(__web_display_list__(f));
assert_isequal(document.is3d, true);
close(f);
%=============================================================================
