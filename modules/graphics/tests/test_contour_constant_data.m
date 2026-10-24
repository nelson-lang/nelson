%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% A surface with no relief carries no contour line to draw. That is a warning,
% not a refusal: the object is built, placed in the axes, and reads back
% normally with an empty LevelList. The two ways relief can be missing keep
% separate warnings, and data holding a single finite value counts as constant,
% not as non-finite. Every expected value below was measured.
%=============================================================================
f = figure('Visible', 'off');
lastwarn('', '');
[C, h] = contour(ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.isequal(msg, 'Contour not rendered for constant ZData.');
asserts.istrue(isgraphics(h));
asserts.isequal(get(h, 'Type'), 'contour');
ax = ancestor(h, 'axes');
asserts.isequal(get(h, 'Parent'), ax);
asserts.isequal(numel(get(ax, 'Children')), 1);
asserts.isequal(size(C), [2 0]);
asserts.istrue(isempty(C));
asserts.isequal(get(h, 'LevelList'), zeros(1, 0));
asserts.isequal(get(h, 'LevelListMode'), 'auto');
asserts.isequal(get(h, 'LevelStep'), 0);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
asserts.isequal(size(get(h, 'ContourMatrix')), [2 0]);
asserts.isequal(get(ax, 'XLim'), [1 10]);
asserts.isequal(get(ax, 'YLim'), [1 10]);
close(f);
%=============================================================================
% The filled and the three-dimensional forms answer the same way.
f = figure('Visible', 'off');
lastwarn('', '');
[C, h] = contourf(ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.istrue(isgraphics(h));
asserts.isequal(size(C), [2 0]);
asserts.isequal(get(h, 'LevelList'), zeros(1, 0));
asserts.isequal(get(h, 'LevelListMode'), 'auto');
asserts.isequal(get(h, 'LevelStep'), 0);
close(f);
%=============================================================================
f = figure('Visible', 'off');
lastwarn('', '');
[C, h] = contour3(ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.istrue(isgraphics(h));
asserts.isequal(size(C), [2 0]);
asserts.isequal(get(h, 'LevelList'), zeros(1, 0));
close(f);
%=============================================================================
% Data with no finite value at all is a different case and says so.
f = figure('Visible', 'off');
lastwarn('', '');
[C, h] = contour(nan(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:nonFiniteData');
asserts.isequal(msg, 'Contour not rendered for non-finite ZData.');
asserts.istrue(isgraphics(h));
asserts.isequal(size(C), [2 0]);
asserts.isequal(get(h, 'LevelList'), zeros(1, 0));
asserts.isequal(get(h, 'LevelListMode'), 'auto');
asserts.isequal(get(h, 'LevelStep'), 0);
close(f);
%=============================================================================
f = figure('Visible', 'off');
lastwarn('', '');
contour(inf(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:nonFiniteData');
close(f);
%=============================================================================
f = figure('Visible', 'off');
lastwarn('', '');
contour(-inf(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:nonFiniteData');
close(f);
%=============================================================================
% One finite value drowned in NaN is constant data, not non-finite data.
Z = nan(10, 10);
Z(3, 4) = 7;
f = figure('Visible', 'off');
lastwarn('', '');
[C, h] = contour(Z);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.isequal(size(C), [2 0]);
asserts.isequal(get(h, 'LevelList'), zeros(1, 0));
close(f);
%=============================================================================
% So is constant data holding a stray NaN or a stray Inf: the value that is not
% finite is left out before the values are compared.
Z = ones(10, 10);
Z(1, 1) = NaN;
f = figure('Visible', 'off');
lastwarn('', '');
contour(Z);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
close(f);
%=============================================================================
Z = ones(10, 10);
Z(1, 1) = Inf;
f = figure('Visible', 'off');
lastwarn('', '');
contour(Z);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
close(f);
%=============================================================================
% A count of levels over a flat range collapses to the single value the data
% sits at, whatever the count asked for, and the list counts as set.
f = figure('Visible', 'off');
[~, h] = contour(ones(10, 10), 5);
asserts.isequal(get(h, 'LevelList'), 1);
asserts.isequal(get(h, 'LevelListMode'), 'manual');
asserts.isequal(get(h, 'LevelStep'), 0);
asserts.isequal(get(h, 'LevelStepMode'), 'auto');
asserts.isequal(size(get(h, 'ContourMatrix')), [2 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(ones(10, 10), 1);
asserts.isequal(get(h, 'LevelList'), 1);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(zeros(10, 10), 2);
asserts.isequal(get(h, 'LevelList'), 0);
asserts.isequal(get(h, 'LevelListMode'), 'manual');
close(f);
%=============================================================================
f = figure('Visible', 'off');
[~, h] = contour(3.5 * ones(6, 7), 3);
asserts.isequal(get(h, 'LevelList'), 3.5);
asserts.isequal(get(h, 'LevelListMode'), 'manual');
close(f);
%=============================================================================
% Over data with no finite value the same count collapses to nothing at all,
% and the list stays automatic rather than turning into an empty manual one.
f = figure('Visible', 'off');
[~, h] = contour(nan(10, 10), 3);
asserts.isequal(get(h, 'LevelList'), zeros(1, 0));
asserts.isequal(get(h, 'LevelListMode'), 'auto');
close(f);
%=============================================================================
% A list spelled out is kept as given, and still draws nothing.
f = figure('Visible', 'off');
[C, h] = contour(ones(10, 10), [1 2 3]);
asserts.isequal(get(h, 'LevelList'), [1 2 3]);
asserts.isequal(get(h, 'LevelListMode'), 'manual');
asserts.isequal(size(C), [2 0]);
close(f);
%=============================================================================
% contourc returns its empty matrix without a word: only the plotting
% functions warn.
lastwarn('', '');
M = contourc(ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isequal(size(M), [2 0]);
asserts.istrue(isempty(M));
%=============================================================================
lastwarn('', '');
M = contourc(nan(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isequal(size(M), [2 0]);
%=============================================================================
lastwarn('', '');
M = contourc(ones(10, 10), [1 2 3]);
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isequal(size(M), [2 0]);
%=============================================================================
% The functions that lay a contour under a surface go through the same door.
f = figure('Visible', 'off');
lastwarn('', '');
h = surfc(ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.isequal(numel(h), 2);
asserts.isequal(get(h(2), 'Type'), 'contour');
asserts.isequal(get(h(2), 'LevelList'), zeros(1, 0));
close(f);
%=============================================================================
f = figure('Visible', 'off');
lastwarn('', '');
h = meshc(ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.isequal(numel(h), 2);
asserts.isequal(get(h(2), 'LevelList'), zeros(1, 0));
close(f);
%=============================================================================
% contourslice cuts through contourc, so a constant volume yields no line and
% no warning.
[Xg, Yg, Zg] = meshgrid(1:5, 1:5, 1:5);
V = ones(size(Xg));
f = figure('Visible', 'off');
lastwarn('', '');
h = contourslice(Xg, Yg, Zg, V, [], [], 3);
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isequal(numel(h), 0);
close(f);
%=============================================================================
% The smallest matrix that can be contoured behaves the same, and explicit
% coordinates still reach the axes limits.
f = figure('Visible', 'off');
lastwarn('', '');
[C, h] = contour(ones(2, 2));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
asserts.isequal(size(C), [2 0]);
ax = ancestor(h, 'axes');
asserts.isequal(get(ax, 'XLim'), [1 2]);
asserts.isequal(get(ax, 'YLim'), [1 2]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
lastwarn('', '');
[~, h] = contour(10:19, 20:29, ones(10, 10));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:contour:constantData');
ax = ancestor(h, 'axes');
asserts.isequal(get(ax, 'XLim'), [10 19]);
asserts.isequal(get(ax, 'YLim'), [20 29]);
close(f);
%=============================================================================
% Data that does have relief stays silent, however little of it there is.
f = figure('Visible', 'off');
lastwarn('', '');
contour(peaks(20));
[msg, id] = lastwarn();
asserts.isequal(id, '');
close(f);
%=============================================================================
Z = ones(10, 10);
Z(1:5, :) = 2;
f = figure('Visible', 'off');
lastwarn('', '');
contour(Z);
[msg, id] = lastwarn();
asserts.isequal(id, '');
close(f);
%=============================================================================
% A matrix too small to be a grid is still refused outright.
f = figure('Visible', 'off');
asserts.checkerror('contour(ones(1, 10))', getString(message('Nelson:validateContourData:invalidZ')));
asserts.checkerror('contour(ones(1, 10))', getString(message('Nelson:validateContourData:invalidZ')), ...
  'Nelson:validateContourData:invalidZ');
close(f);
%=============================================================================
