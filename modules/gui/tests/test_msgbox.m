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
assert_isequal(nargin('msgbox'), -1);
assert_isequal(nargout('msgbox'), -1);
%=============================================================================
h = msgbox('message');
h = msgbox({'Operation' 'Completed'});
h = msgbox('message', 'modal');
h = msgbox('message', 'nonmodal');
h = msgbox('message', 'on');
h = msgbox('message', 'title');
h = msgbox('message', 'title', 'modal');
h = msgbox('message', 'title', 'nonmodal');
h = msgbox('message', 'title', 'on');
h = msgbox('message', 'title', 'none');
h = msgbox('message', 'title', 'error');
h = msgbox('message', 'title', 'help');
h = msgbox('message', 'title', 'warn');
h = msgbox('message', 'title', 'question');
h = msgbox('message', 'title', 'none', 'modal');
h = msgbox('message', 'title', 'error', 'nonmodal');
h = msgbox('message', 'title', 'help', 'on')
assert_checkerror('h = msgbox(''message'', ''title'', ''toto'')', _('Wrong value for #3 argument. A valid icon or mode expected.'));
assert_checkerror('h = msgbox(''message'', ''title'', ''warn'', ''one'')', _('Wrong value for #4 argument. A valid mode expected.'));
assert_checkerror('h = msgbox(''message'', ''title'', ''question'', ''two'')', _('Wrong value for #4 argument. A valid mode expected.'));
