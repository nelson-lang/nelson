%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--GUI MODE-->
%=============================================================================
asserts.isequal(nargin('errordlg'), 3);
asserts.isequal(nargout('errordlg'), -1);
%=============================================================================
h = errordlg()
asserts.isequal(class(h), 'nelson.ui.Figure')
asserts.istrue(isgraphics(h, 'figure'))
asserts.isequal(h.Tag, 'Msgbox_Error Dialog')
button = findobj(h, 'Tag', 'OKButton');
asserts.isequal(class(button), 'nelson.ui.control.UIControl')
message = findobj(h, 'Tag', 'MessageBox');
asserts.isequal(class(message), 'nelson.graphics.primitive.Text')
asserts.isequal(message.String, 'This is the default error.')
icon = findobj(h, 'Tag', 'IconAxes');
asserts.isequal(class(icon), 'nelson.graphics.axis.Axes')
asserts.istrue(isgraphics(icon, 'axes'))
asserts.isequal(char(icon.Visible), 'off')
image = findobj(icon, 'Type', 'image');
asserts.isequal(class(image), 'nelson.graphics.primitive.Image')
asserts.isequal(numel(image), 1)
close(h)
h = errordlg('errorstring')
asserts.isequal(class(h), 'nelson.ui.Figure')
asserts.istrue(isgraphics(h, 'figure'))
asserts.isequal(h.Tag, 'Msgbox_Error Dialog')
icon = findobj(h, 'Tag', 'IconAxes');
asserts.istrue(isgraphics(icon, 'axes'))
asserts.isequal(char(icon.Visible), 'off')
asserts.isequal(numel(findobj(icon, 'Type', 'image')), 1)
close(h)
h = errordlg('errorstring', 'dlgname')
asserts.isequal(class(h), 'nelson.ui.Figure')
asserts.istrue(isgraphics(h, 'figure'))
asserts.isequal(h.Tag, 'Msgbox_dlgname')
icon = findobj(h, 'Tag', 'IconAxes');
asserts.istrue(isgraphics(icon, 'axes'))
asserts.isequal(char(icon.Visible), 'off')
asserts.isequal(numel(findobj(icon, 'Type', 'image')), 1)
close(h)
h = errordlg('errorstring', 'dlgname', 'on')
asserts.isequal(class(h), 'nelson.ui.Figure')
asserts.istrue(isgraphics(h, 'figure'))
asserts.isequal(h.Tag, 'Msgbox_dlgname')
icon = findobj(h, 'Tag', 'IconAxes');
asserts.istrue(isgraphics(icon, 'axes'))
asserts.isequal(char(icon.Visible), 'off')
asserts.isequal(numel(findobj(icon, 'Type', 'image')), 1)
close(h)
%=============================================================================
