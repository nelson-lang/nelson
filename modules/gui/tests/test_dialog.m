%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
asserts.isequal(nargin('dialog'), -1);
asserts.isequal(nargout('dialog'), 1);
%=============================================================================
f = dialog('Visible', 'off', 'Name', 'test_dialog');
asserts.istrue(isgraphics(f, 'figure'));
asserts.isequal(f.MenuBar, 'none');
asserts.isequal(f.ToolBar, 'auto');
asserts.isequal(char(f.NumberTitle), 'off');
asserts.isequal(char(f.Resize), 'off');
asserts.isequal(f.WindowStyle, 'modal');
asserts.isequal(f.Name, 'test_dialog');
close(f);
%=============================================================================
