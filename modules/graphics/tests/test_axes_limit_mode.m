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
reset(groot());
%=============================================================================
% A fresh axes scales itself: every limit and tick mode starts on 'auto'.
f = figure('Visible', 'off');
ax = axes('Parent', f);
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YLimMode'), 'auto');
asserts.isequal(get(ax, 'ZLimMode'), 'auto');
asserts.isequal(get(ax, 'XTickMode'), 'auto');
asserts.isequal(get(ax, 'YTickMode'), 'auto');
asserts.isequal(get(ax, 'XTickLabelMode'), 'auto');
%=============================================================================
% Assigning a value pins the matching mode to 'manual'.
set(ax, 'XLim', [-30 0]);
set(ax, 'YLim', [0 1]);
set(ax, 'ZLim', [-2 2]);
set(ax, 'XTick', [-20 -10]);
set(ax, 'YTick', []);
set(ax, 'XTickLabel', {'a', 'b'});
asserts.isequal(get(ax, 'XLimMode'), 'manual');
asserts.isequal(get(ax, 'YLimMode'), 'manual');
asserts.isequal(get(ax, 'ZLimMode'), 'manual');
asserts.isequal(get(ax, 'XTickMode'), 'manual');
asserts.isequal(get(ax, 'YTickMode'), 'manual');
asserts.isequal(get(ax, 'XTickLabelMode'), 'manual');
close(f);
%=============================================================================
% A pinned axes keeps its limits and ticks when an image is added. image() in
% its low-level property/value form does not go through newplot(), so it must
% not send the modes back to 'auto' behind the caller's back.
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'XLim', [-30 0], 'YLim', [0 1], 'XTick', [], 'YTick', []);
im = image('Parent', ax, 'CData', rand(4, 5, 3), 'XData', [-30 0], 'YData', [0 1]);
asserts.isequal(get(im, 'Parent'), ax);
asserts.isequal(get(ax, 'XLim'), [-30 0]);
asserts.isequal(get(ax, 'YLim'), [0 1]);
asserts.isequal(numel(get(ax, 'XTick')), 0);
asserts.isequal(numel(get(ax, 'YTick')), 0);
asserts.isequal(get(ax, 'XLimMode'), 'manual');
asserts.isequal(get(ax, 'YLimMode'), 'manual');
asserts.isequal(get(ax, 'XTickMode'), 'manual');
close(f);
%=============================================================================
% Same thing with the modes pinned explicitly rather than through a value.
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'XLimMode', 'manual', 'YLimMode', 'manual');
set(ax, 'XLim', [-30 0], 'YLim', [0 1]);
image('Parent', ax, 'CData', rand(4, 5, 3), 'XData', [-30 0], 'YData', [0 1]);
asserts.isequal(get(ax, 'XLim'), [-30 0]);
asserts.isequal(get(ax, 'YLim'), [0 1]);
close(f);
%=============================================================================
% A line never triggered the reset, and still does not.
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'XLim', [-30 0], 'YLim', [0 1], 'XTick', []);
line('Parent', ax, 'XData', [-30 0], 'YData', [0.2 0.8]);
asserts.isequal(get(ax, 'XLim'), [-30 0]);
asserts.isequal(get(ax, 'YLim'), [0 1]);
asserts.isequal(numel(get(ax, 'XTick')), 0);
close(f);
%=============================================================================
% Handing the axes back to 'auto' resumes the automatic scaling.
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'XLim', [-30 0], 'YLim', [0 1]);
image('Parent', ax, 'CData', rand(4, 5, 3), 'XData', [-30 0], 'YData', [0 1]);
asserts.isequal(get(ax, 'XLim'), [-30 0]);
set(ax, 'XLimMode', 'auto');
set(ax, 'YLimMode', 'auto');
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(get(ax, 'YLimMode'), 'auto');
line('Parent', ax, 'XData', [-100 100], 'YData', [-5 5]);
asserts.isequal(get(ax, 'XLim'), [-100 100]);
asserts.isequal(get(ax, 'YLim'), [-5 5]);
close(f);
%=============================================================================
% The high-level forms still replace what the axes holds.
f = figure('Visible', 'off');
ax = axes('Parent', f);
for k = 1:3
  set(f, 'CurrentAxes', ax);
  image(rand(4, 5, 3));
  asserts.isequal(numel(get(ax, 'Children')), 1);
end
close(f);
%=============================================================================
close all;
%=============================================================================
