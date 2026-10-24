%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('native tab reparenting layout differs in portable rendering.');
f = uifigure('Visible', 'on', 'Position', [100 100 480 300]);
first = uitabgroup(f, 'Position', [10 10 220 280]);
second = uitabgroup(f, 'Position', [250 10 220 280]);
page = uitab(first, 'Title', 'Movable');
marker = uilabel(page, 'Text', '', 'BackgroundColor', [1 0 0], ...
  'Position', [30 40 120 100]);
drawnow();
page.Parent = second;
second.SelectedTab = page;
drawnow();
pause(0.1);
frame = getframe(f);
redPixels = frame.cdata(:, :, 1) > 200 & ...
  frame.cdata(:, :, 2) < 80 & frame.cdata(:, :, 3) < 80;
asserts.isequal(page.Parent, second);
asserts.isequal(numel(first.Children), 0);
asserts.isequal(numel(second.Children), 1);
asserts.istrue(isempty(first.SelectedTab));
asserts.isequal(second.SelectedTab, page);
asserts.istrue(isgraphics(marker));
asserts.istrue(nnz(redPixels) > 100);
page.Parent = first;
first.SelectedTab = page;
drawnow();
pause(0.1);
frame = getframe(f);
redPixels = frame.cdata(:, :, 1) > 200 & ...
  frame.cdata(:, :, 2) < 80 & frame.cdata(:, :, 3) < 80;
asserts.isequal(numel(first.Children), 1);
asserts.isequal(numel(second.Children), 0);
asserts.isequal(first.SelectedTab, page);
asserts.istrue(isempty(second.SelectedTab));
asserts.istrue(nnz(redPixels) > 100);
close(f);
%=============================================================================
