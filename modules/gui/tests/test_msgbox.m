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
asserts.isequal(nargin('msgbox'), -1);
asserts.isequal(nargout('msgbox'), -1);
%=============================================================================
h = msgbox('message');
asserts.isequal(class(h), 'nelson.ui.Figure');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox({'Operation' 'Completed'});
asserts.isequal(class(h), 'nelson.ui.Figure');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'modal');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'nonmodal');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'on');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title', 'modal');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title', 'nonmodal');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title', 'on');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title', 'none');
asserts.istrue(isgraphics(h, 'figure'));
asserts.istrue(isempty(findobj(h, 'Tag', 'IconAxes')));
close(h);
h = msgbox('message', 'title', 'error');
asserts.istrue(isgraphics(h, 'figure'));
asserts.isequal(h.Tag, 'Msgbox_title');
button = findobj(h, 'Tag', 'OKButton');
asserts.isequal(class(button), 'nelson.ui.control.UIControl');
icon = findobj(h, 'Tag', 'IconAxes');
asserts.isequal(class(icon), 'nelson.graphics.axis.Axes');
asserts.istrue(isgraphics(icon, 'axes'));
image = findobj(icon, 'Type', 'image');
asserts.isequal(class(image), 'nelson.graphics.primitive.Image');
asserts.isequal(numel(image), 1);
close(h);
h = msgbox('message', 'title', 'help');
asserts.istrue(isgraphics(h, 'figure'));
icon = findobj(h, 'Tag', 'IconAxes');
asserts.istrue(isgraphics(icon, 'axes'));
asserts.isequal(numel(findobj(icon, 'Type', 'image')), 1);
close(h);
h = msgbox('message', 'title', 'warn');
asserts.istrue(isgraphics(h, 'figure'));
icon = findobj(h, 'Tag', 'IconAxes');
asserts.istrue(isgraphics(icon, 'axes'));
asserts.isequal(numel(findobj(icon, 'Type', 'image')), 1);
close(h);
h = msgbox('message', 'title', 'question');
asserts.istrue(isgraphics(h, 'figure'));
icon = findobj(h, 'Tag', 'IconAxes');
asserts.istrue(isgraphics(icon, 'axes'));
asserts.isequal(numel(findobj(icon, 'Type', 'image')), 1);
close(h);
h = msgbox('message', 'title', 'none', 'modal');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title', 'error', 'nonmodal');
asserts.istrue(isgraphics(h, 'figure'));
close(h);
h = msgbox('message', 'title', 'help', 'on')
asserts.istrue(isgraphics(h, 'figure'));
close(h);
asserts.checkerror('h = msgbox(''message'', ''title'', ''toto'')', sprintf(_('Wrong value for #%d argument. A valid icon or mode expected.'), 3));
asserts.checkerror('h = msgbox(''message'', ''title'', ''warn'', ''one'')', sprintf(_('Wrong value for #%d argument. A valid mode expected.'), 4));
asserts.checkerror('h = msgbox(''message'', ''title'', ''question'', ''two'')', sprintf(_('Wrong value for #%d argument. A valid mode expected.'), 4));
