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
% <--GUI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('checkbox getframe colors differ in portable rendering.');
% Non-regression: a check box has no BackgroundColor property, so it must blend
% with its parent. Previously a style sheet made Qt paint the default (light)
% widget background, which hid a light FontColor on a dark figure.
%=============================================================================
f = uifigure('Position', [100 100 300 120], 'Color', [0.05 0.05 0.08]);
uicheckbox(f, 'Text', 'Assisted breaths', 'Value', true, 'FontColor', [1 1 0], ...
  'Position', [20 50 220 24]);
drawnow();
F = getframe(f);
close(f);
C = double(F.cdata);
R = C(:, :, 1);
G = C(:, :, 2);
B = C(:, :, 3);
% The yellow label is painted (not hidden behind a light background).
yellow = nnz(R > 200 & G > 200 & B < 60);
assert_istrue(yellow > 50);
% No large light-gray background bar behind the label: only the small white
% indicator box stays light.
grayBackground = nnz(min(C, [], 3) > 170 & (max(C, [], 3) - min(C, [], 3)) < 25);
assert_istrue(grayBackground < 1500);
%=============================================================================
