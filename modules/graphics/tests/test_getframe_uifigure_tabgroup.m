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
% getframe on a uifigure that hosts a uitabgroup must capture the window
% without error and return a non-blank frame. A uitabgroup (and its uitab
% children) on the capture path previously broke the figure/axes handle test.
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 400 300], ...
  'Color', [0 0 0]);
tabGroup = uitabgroup(fig, 'Position', [10 10 380 280]);
firstTab = uitab(tabGroup, 'Title', 'A');
uitab(tabGroup, 'Title', 'B');
uibutton(firstTab, 'Text', 'hi', 'Position', [20 20 80 30]);
drawnow();

frame = getframe(fig);
assert_istrue(isstruct(frame));
cdata = frame.cdata;
assert_isequal(length(size(cdata)), 3);
assert_isequal(size(cdata, 3), 3);
assert_isequal(class(cdata), 'uint8');
assert_istrue(size(cdata, 1) > 100 && size(cdata, 2) > 100);
% The frame must not be a single uniform color (blank capture).
assert_istrue(double(max(cdata(:))) - double(min(cdata(:))) > 20);
close(fig);
%=============================================================================
