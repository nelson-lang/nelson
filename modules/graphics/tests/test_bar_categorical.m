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
close all
%=============================================================================
% Bars follow the category order (categories()), not the data order, and Y is
% reordered to match.
f = figure('Visible', 'off');
X = categorical({'small', 'medium', 'large'});
b = bar(X, [10 25 15]);
asserts.isequal(b.XData, [1 2 3]);
asserts.isequal(b.YData, [15 25 10]);
ax = gca;
asserts.isequal(class(ax.XTick), 'categorical');
asserts.isequal(ax.XTickLabel, {'large'; 'medium'; 'small'});
close(f);
%=============================================================================
% Reordered categories keep data order aligned with category order.
f = figure('Visible', 'off');
X = categorical({'small', 'medium', 'large'});
X = reordercats(X, {'small', 'medium', 'large'});
b = bar(X, [10 25 15]);
asserts.isequal(b.XData, [1 2 3]);
asserts.isequal(b.YData, [10 25 15]);
ax = gca;
asserts.isequal(ax.XTickLabel, {'small'; 'medium'; 'large'});
close(f);
%=============================================================================
% Grouped (multi-series) bars reorder every series consistently.
f = figure('Visible', 'off');
X = categorical({'b', 'a', 'c'});
b = bar(X, [1 2; 3 4; 5 6]);
asserts.isequal(b(1).XData, [1 2 3]);
asserts.isequal(b(1).YData, [3 1 5]);
asserts.isequal(b(2).YData, [4 2 6]);
ax = gca;
asserts.isequal(ax.XTickLabel, {'a'; 'b'; 'c'});
close(f);
%=============================================================================
close all
%=============================================================================
