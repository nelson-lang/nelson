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
f = uicontrol('Style', 'text', 'String', 'Hello World!', 'Position', [100, 100, 100, 50]);
assert_isequal(f.String, 'Hello World!');
pause(2);
f.String = 'Bye World!';
assert_isequal(f.String, 'Bye World!');
pause(2);
%=============================================================================
