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
close all
%=============================================================================
% camlight creates a LOCAL light near the camera, offset from the camera-to-
% target direction by azimuth/elevation (matching the reference). The headlight
% sits exactly at the camera position.
f = figure('Visible', 'off');
ax = axes('Parent', f);
surf(ax, peaks());
set(ax, 'CameraPosition', [2 0 0], 'CameraTarget', [10 0 0], 'CameraUpVector', [0 0 1]);
%=============================================================================
h = camlight(ax, 'headlight');
asserts.istrue(isgraphics(h, 'light'));
asserts.isequal(char(string(h.Style)), 'local');
asserts.isapprox(h.Position, [2 0 0], 1e-3);
delete(h);
%=============================================================================
h = camlight(ax, 'right');
asserts.isequal(char(string(h.Style)), 'local');
asserts.isapprox(h.Position, [4 -3.4641 4], 1e-3);
delete(h);
%=============================================================================
h = camlight(ax, 'left');
asserts.istrue(isgraphics(h, 'light'));
asserts.isequal(char(string(h.Style)), 'local');
asserts.isapprox(h.Position, [4 3.4641 4], 1e-3);
delete(h);
%=============================================================================
% the no-argument form defaults to 'right'.
h = camlight(ax);
asserts.isapprox(h.Position, [4 -3.4641 4], 1e-3);
close all
%=============================================================================
% a trailing 'local' or 'infinite' selects the light style, with any position
f = figure('Visible', 'off');
ax = axes('Parent', f);
surf(peaks(10));
h = camlight('headlight', 'infinite');
asserts.isequal(h.Style, 'infinite');
h2 = camlight('right', 'local');
asserts.isequal(h2.Style, 'local');
% style given on its own keeps the default 'right' position
h3 = camlight('infinite');
asserts.isequal(h3.Style, 'infinite');
% default style stays local
h4 = camlight('headlight');
asserts.isequal(h4.Style, 'local');
close(f);
%=============================================================================
