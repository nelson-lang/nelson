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
f = figure();
h = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Units', 'pixels');
h.Position = [100, 100, 100, 50];
assert_isequal(h.Position, [100, 100, 100, 50]);
assert_isequal(h.Units, 'pixels');
h.Units = 'normalized';
assert_isequal(h.Units, 'normalized');
assert_isapprox(h.Position, [0.178571 0.212766 0.178571 0.106383], 1e-3);
h.Units = 'pixels';
assert_isequal(h.Units, 'pixels');
assert_isequal(h.Position, [100, 100, 100, 50]);
%=============================================================================
f = figure();
h = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Units', 'normalized');
h.Position = [0.178571 0.212766 0.178571 0.106383];
assert_isequal(h.Units, 'normalized');
assert_isapprox(h.Position, [0.178571 0.212766 0.178571 0.106383], 1e-3);
h.Units = 'pixels';
assert_isequal(h.Units, 'pixels');
assert_isapprox(h.Position, [   100   100   100    50], 1e-3);
%=============================================================================
