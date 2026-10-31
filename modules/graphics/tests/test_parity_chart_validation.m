%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Chart validation parity, and axes(f) creating new axes.
%=============================================================================
f = figure('Visible', 'off');
h = heatmap(magic(4));
%=============================================================================
assert_istrue(setFails(h, 'ColorLimits', [5 1]));
assert_istrue(setFails(h, 'ColorLimits', [1 2 3]));
assert_istrue(setFails(h, 'CellLabelFormat', ''));
assert_istrue(setFails(h, 'FontSize', -3));
assert_istrue(setFails(h, 'FontSize', 'a'));
h.ColorLimits = [0 20];
assert_isequal(h.ColorLimits, [0 20]);
h.MissingDataLabel = 3;
assert_isequal(h.MissingDataLabel, '3');
%=============================================================================
% display labels must match the data size, Colormap is an Mx3 array in [0 1], ColorMethod is
% one of the known methods, and a number is accepted as text for the labels
assert_istrue(setFails(h, 'XDisplayLabels', {'a', 'b'}));
assert_istrue(setFails(h, 'YDisplayLabels', {'a', 'b', 'c', 'd', 'e'}));
h.XDisplayLabels = {'a', 'b', 'c', 'd'};
assert_isequal(numel(h.XDisplayLabels), 4);
assert_istrue(setFails(h, 'Colormap', 'bad'));
assert_istrue(setFails(h, 'Colormap', [1 0 0; 0 1 0]'));
assert_istrue(setFails(h, 'Colormap', [2 0 0]));
h.Colormap = [1 0 0; 0 1 0];
assert_isequal(size(h.Colormap), [2 3]);
assert_istrue(setFails(h, 'ColorMethod', 'x'));
h.ColorMethod = 'count';
assert_isequal(h.ColorMethod, 'count');
h.Title = 3;
assert_isequal(h.Title, '3');
h.XLabel = 3.5;
assert_isequal(h.XLabel, '3.5');
close(f);
%=============================================================================
% a heatmap too dense for its labels creates no label objects; its grid is one line
f = figure('Visible', 'off');
h = heatmap(rand(40, 40));
state = h.UserData;
assert_isequal(numel(state.CellLabels), 0);
assert_isequal(numel(state.Grid), 1);
close(f);
%=============================================================================
f = figure('Visible', 'off');
words = {'alpha', 'beta', 'gamma', 'delta'};
w = wordcloud(words, [4 3 2 1]);
assert_isequal(w.Position, [0 0 1 1]);
assert_istrue(setFails(w, 'MaxDisplayWords', 0));
assert_istrue(setFails(w, 'SizePower', -1));
assert_istrue(setFails(w, 'LayoutNum', -1));
assert_istrue(setFails(w, 'MaxDisplayWords', 2.5));
assert_istrue(setFails(w, 'MaxDisplayWords', 'a'));
assert_istrue(setFails(w, 'LayoutNum', 1.5));
w.MaxDisplayWords = 3;
assert_isequal(w.MaxDisplayWords, 3);
w.LayoutNum = 2;
assert_isequal(w.LayoutNum, 2);
colors = [1 0 0; 0 1 0; 0 0 1; 0 0 0];
w.Color = colors;
assert_isequal(w.Color, colors);
assert_istrue(setFails(w, 'Color', [2 0 0; 0 1 0]));
w.Title = 7;
assert_isequal(w.Title, '7');
close(f);
%=============================================================================
% the wordcloud constructor also rejects non-integer MaxDisplayWords / LayoutNum
f = figure('Visible', 'off');
assert_istrue(ctorFails({'a', 'b', 'c'}, [3 2 1], 'MaxDisplayWords', 2.5));
assert_istrue(ctorFails({'a', 'b', 'c'}, [3 2 1], 'LayoutNum', 1.5));
assert_istrue(ctorFails({'a', 'b', 'c'}, [3 2 1], 'MaxDisplayWords', -1));
wc = wordcloud({'a', 'b', 'c'}, [3 2 1], 'MaxDisplayWords', 2, 'LayoutNum', 3);
assert_isequal(wc.MaxDisplayWords, 2);
assert_isequal(wc.LayoutNum, 3);
close(f);
%=============================================================================
% axes(f) creates new axes instead of returning the current ones
f = figure('Visible', 'off');
a1 = axes(f);
a2 = axes(f);
assert_istrue(~(a1 == a2));
assert_isequal(numel(findobj(f, 'Type', 'axes')), 2);
close(f);
%=============================================================================
function failed = setFails(obj, name, value)
  failed = false;
  try
    obj.(name) = value;
  catch
    failed = true;
  end
end
%=============================================================================
function failed = ctorFails(varargin)
  failed = false;
  try
    wordcloud(varargin{:});
  catch
    failed = true;
  end
end
%=============================================================================
