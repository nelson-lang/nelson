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
asserts.isequal(nargin('uifigure'), -1);
asserts.isequal(nargout('uifigure'), 1);
%=============================================================================
f = uifigure('Visible', 'off');
asserts.istrue(isgraphics(f, 'figure'));
asserts.isequal(f.MenuBar, 'none');
asserts.isequal(f.ToolBar, 'none');
asserts.isequal(char(f.NumberTitle), 'off');
asserts.isequal(char(f.Resize), 'on');
asserts.isequal(f.WindowStyle, 'normal');
asserts.isequal(char(f.AutoResizeChildren), 'on');
asserts.isequal(char(f.HandleVisibility), 'off');
asserts.isequal(char(f.IntegerHandle), 'off');
asserts.istrue(isempty(f.Number));
asserts.isequal(f.Name, '');
asserts.isequal(f.Units, 'pixels');
asserts.isequal(f.Position, [488 242 560 420]);
close(f);
%=============================================================================
f = uifigure('Visible', 'off', 'Name', 'Results', 'WindowStyle', 'modal');
asserts.isequal(f.Name, 'Results');
asserts.isequal(f.WindowStyle, 'modal');
close(f);
%=============================================================================
