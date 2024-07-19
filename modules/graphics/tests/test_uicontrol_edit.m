%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
h = uicontrol('Style', 'edit', 'String', 'Click Me!', 'Position', [100, 100, 100, 50]);
assert_isequal(h.String, 'Click Me!');
pause(2);
h.String = 'Hello World!';
assert_isequal(h.String, 'Hello World!');
pause(2);
%=============================================================================
