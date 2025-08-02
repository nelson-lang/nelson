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
h = uicontrol('Style', 'popupmenu', 'String', {'Option 1', 'Option 2', 'Option 3'}, 'Position', [100, 100, 100, 50]);
assert_isequal(h.Value, 1);
pause(2);
h.Value = 2;
assert_isequal(h.Value, 2);
pause(2);
h.Value = 3;
assert_isequal(h.Value, 3);
pause(2);
%=============================================================================
