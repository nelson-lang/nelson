%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% A LevelStep nobody set reports the round step the automatic levels are
% multiples of, while LevelStepMode stays 'auto'. Every expected step below
% was measured, none was derived.
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(5));
asserts.isequal(get(h, 'LevelStep'), 2);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
close(f);
%=============================================================================
% peaks(6) spans barely more than ten units and keeps a step of one.
f = figure('Visible', 'off');
[~, h] = contour(peaks(6));
asserts.isequal(get(h, 'LevelStep'), 1);
close(f);
%=============================================================================
% The step follows the data range through a change of scale, in both
% directions, and stays exact below one.
f = figure('Visible', 'off');
[~, h] = contour(peaks(20) * 10);
asserts.isequal(get(h, 'LevelStep'), 20);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(20) / 10);
asserts.isequal(get(h, 'LevelStep'), 0.2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(magic(10) / 1000);
asserts.isequal(get(h, 'LevelStep'), 0.01);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour([1 2; 3 4]);
asserts.isequal(get(h, 'LevelStep'), 0.5);
close(f);
%=============================================================================
% The range decides, not where the data sits: shifting it by a hundred moves
% no step, and data that is entirely negative is read the same way.
f = figure('Visible', 'off');
[~, h] = contour(peaks(20) + 100);
asserts.isequal(get(h, 'LevelStep'), 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(magic(10));
asserts.isequal(get(h, 'LevelStep'), 10);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(-abs(magic(10)));
asserts.isequal(get(h, 'LevelStep'), 10);
close(f);
%=============================================================================
% The step is republished when the data changes under an automatic contour.
f = figure('Visible', 'off');
[~, h] = contour(peaks(20));
asserts.isequal(get(h, 'LevelStep'), 2);
set(h, 'ZData', peaks(20) * 10);
asserts.isequal(get(h, 'LevelStep'), 20);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
close(f);
%=============================================================================
% A LevelList given as a count of levels leaves LevelStep on the step of the
% range: it is not the spacing of that list, whatever the count.
f = figure('Visible', 'off');
[~, h] = contour(peaks(20), 10);
asserts.isequal(get(h, 'LevelStep'), 2);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
asserts.isequal(get(h, 'LevelListMode'), 'manual');
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(20), 3);
asserts.isequal(get(h, 'LevelStep'), 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(20), 30);
asserts.isequal(get(h, 'LevelStep'), 2);
close(f);
%=============================================================================
% Same for a list spelled out, given at creation or set afterwards.
f = figure('Visible', 'off');
[~, h] = contour(peaks(20), [0 1 2]);
asserts.isequal(get(h, 'LevelStep'), 2);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(peaks(20));
set(h, 'LevelList', [0 1]);
asserts.isequal(get(h, 'LevelStep'), 2);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
asserts.isequal(get(h, 'LevelListMode'), 'manual');
close(f);
%=============================================================================
% A LevelStep the user did set is kept, and switches the mode to manual.
f = figure('Visible', 'off');
[~, h] = contour(peaks(20), 'LevelStep', 3);
asserts.isequal(get(h, 'LevelStep'), 3);
asserts.isequal(get(h, 'LevelStepMode'), 'manual');
asserts.isequal(get(h, 'LevelList'), [-6 -3 0 3 6]);
close(f);
%=============================================================================
% The filled contour reports the step of the range, not the spacing of the
% synthetic base band its LevelList starts with.
f = figure('Visible', 'off');
[~, h] = contourf(peaks(20));
asserts.isequal(get(h, 'LevelStep'), 2);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour3(peaks(20));
asserts.isequal(get(h, 'LevelStep'), 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fcontour(@(x, y) x .^ 2 + y .^ 2);
asserts.isequal(get(h, 'LevelStep'), 5);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
close(f);
%=============================================================================
% The contour a surfc projects under its surface reports it too.
f = figure('Visible', 'off');
h = surfc(peaks(20));
asserts.isequal(get(h(2), 'LevelStep'), 2);
asserts.isequal(get(h(2), 'LevelStepMode'), 'auto');
close(f);
%=============================================================================
