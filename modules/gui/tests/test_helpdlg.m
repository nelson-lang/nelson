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
asserts.isequal(nargin('helpdlg'), 2);
asserts.isequal(nargout('helpdlg'), -1);
%=============================================================================
h = helpdlg();
asserts.isequal(class(h), 'nelson.ui.Figure');
asserts.istrue(isgraphics(h, 'figure'));
asserts.isequal(h.Tag, 'Msgbox_Help Dialog');
button = findobj(h, 'Tag', 'OKButton');
asserts.isequal(class(button), 'nelson.ui.control.UIControl');
icon = findobj(h, 'Tag', 'IconAxes');
asserts.isequal(class(icon), 'nelson.graphics.axis.Axes');
asserts.istrue(isgraphics(icon, 'axes'));
image = findobj(icon, 'Type', 'image');
asserts.isequal(class(image), 'nelson.graphics.primitive.Image');
asserts.isequal(numel(image), 1);
close(h);
%=============================================================================
h = helpdlg('help string', 'help title');
asserts.istrue(isgraphics(h, 'figure'));
asserts.isequal(h.Name, 'help title');
close(h);
%=============================================================================
asserts.checkerror('helpdlg(''a'', ''b'', ''c'')', _('Too many input arguments.'));
%=============================================================================
