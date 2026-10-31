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
% A grid *Mode flag is 'auto' until the matching value is set, which flips it
% to 'manual'. Setting the mode back to 'auto' keeps the value (verified against
% the reference: GridAlpha stays at the manual value after GridAlphaMode='auto').
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10, 1:10);
grid(ax, 'on');
grid(ax, 'minor');
drawnow();
%=============================================================================
% defaults (match the reference)
assert_isequal(char(ax.GridAlphaMode), 'auto');
assert_isequal(ax.GridAlpha, 0.15);
assert_isequal(char(ax.GridLineWidthMode), 'auto');
assert_isequal(ax.GridLineWidth, 0.5);
assert_isequal(char(ax.MinorGridAlphaMode), 'auto');
assert_isequal(ax.MinorGridAlpha, 0.15);
assert_isequal(char(ax.MinorGridLineWidthMode), 'auto');
assert_isequal(ax.MinorGridLineWidth, 0.5);
%=============================================================================
% setting GridAlpha flips the mode to manual and changes the render
dl0 = char(__web_display_list__(f));
ax.GridAlpha = 0.9;
assert_isequal(char(ax.GridAlphaMode), 'manual');
drawnow();
dl1 = char(__web_display_list__(f));
assert_istrue(~isequal(dl0, dl1));
%=============================================================================
% mode back to 'auto' keeps alpha and color but resets line widths to 0.5 (the reference)
ax.GridAlphaMode = 'auto';
drawnow();
assert_isequal(ax.GridAlpha, 0.9);
assert_isequal(char(ax.GridAlphaMode), 'auto');
%=============================================================================
ax.GridLineWidth = 3.0;
assert_isequal(char(ax.GridLineWidthMode), 'manual');
ax.GridLineWidthMode = 'auto';
assert_isequal(ax.GridLineWidth, 0.5);
%=============================================================================
ax.MinorGridAlpha = 0.8;
assert_isequal(char(ax.MinorGridAlphaMode), 'manual');
ax.MinorGridAlphaMode = 'auto';
assert_isequal(ax.MinorGridAlpha, 0.8);
%=============================================================================
ax.MinorGridLineWidth = 2.5;
assert_isequal(char(ax.MinorGridLineWidthMode), 'manual');
ax.MinorGridLineWidthMode = 'auto';
assert_isequal(ax.MinorGridLineWidth, 0.5);
%=============================================================================
close(f);
%=============================================================================
