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
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10, 1:10);
grid(ax, 'on');
%=============================================================================
% default Layer is 'bottom': the grid is drawn under the data line
assert_isequal(char(ax.Layer), 'bottom');
dl = char(__web_display_list__(f));
assert_istrue(min(strfind(dl, 'rgba(221,221,221')) < min(strfind(dl, 'rgba(16,112,189')));
%=============================================================================
% Layer 'top' draws the grid over the data line
ax.Layer = 'top';
grid(ax, 'off');
grid(ax, 'on');
dl = char(__web_display_list__(f));
assert_istrue(min(strfind(dl, 'rgba(221,221,221')) > min(strfind(dl, 'rgba(16,112,189')));
%=============================================================================
close(f);
%=============================================================================
