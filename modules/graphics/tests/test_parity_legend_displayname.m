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
% Legend parity with the reference: String is a row, explicit labels are written
% to DisplayName and DisplayName changes follow, ItemTokenSize, default ItemHitFcn.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
a = plot(ax, 1:3);
b = plot(ax, 3:-1:1);
lg = legend(ax);
assert_isequal(size(lg.String), [1 2]);
assert_isequal(lg.String, {'data1', 'data2'});
assert_isequal(class(lg.ItemHitFcn), 'function_handle');
%=============================================================================
lg = legend(ax, {'x', 'y'});
assert_isequal(lg.String, {'x', 'y'});
assert_isequal(a.DisplayName, 'x');
assert_isequal(b.DisplayName, 'y');
a.DisplayName = 'AA';
drawnow();
assert_isequal(lg.String, {'AA', 'y'});
lg.String = {'p', 'q'};
drawnow();
assert_isequal(a.DisplayName, 'p');
assert_isequal(b.DisplayName, 'q');
%=============================================================================
% an unnamed new object gets the next generated name
c = plot(ax, [1 1 1]);
drawnow();
assert_isequal(lg.String, {'p', 'q', 'data1'});
%=============================================================================
assert_isequal(lg.ItemTokenSize, [30; 18]);
lg.ItemTokenSize = [20 10];
assert_isequal(lg.ItemTokenSize, [20; 10]);
close(f);
%=============================================================================
