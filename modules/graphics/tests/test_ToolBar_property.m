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
asserts.isequal(f.ToolBar, 'auto');
f.ToolBar = 'none';
asserts.isequal(f.ToolBar, 'none');
drawnow();
pause(0.1);
f.ToolBar = 'figure';
asserts.isequal(f.ToolBar, 'figure');
drawnow();
pause(0.1);
f.ToolBar = 'auto';
asserts.isequal(f.ToolBar, 'auto');
%=============================================================================
