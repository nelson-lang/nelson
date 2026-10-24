%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('usejava'), 1);
assert_isequal(nargout('usejava'), 1);
%=============================================================================
% Nelson embeds no JVM: every supported feature returns logical false.
r = usejava('jvm');
assert_istrue(islogical(r));
assert_isequal(r, false);
%=============================================================================
assert_isequal(usejava('awt'), false);
assert_isequal(usejava('swing'), false);
%=============================================================================
% string scalar input is accepted.
assert_isequal(usejava("jvm"), false);
%=============================================================================
% 'desktop' is not a supported feature: it is treated as invalid.
assert_checkerror('usejava(''desktop'')', _('Feature must be one of the following: ''awt'', ''jvm'', ''swing''.'));
%=============================================================================
assert_checkerror('usejava(''unknown'')', _('Feature must be one of the following: ''awt'', ''jvm'', ''swing''.'));
%=============================================================================
assert_checkerror('usejava(1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
%=============================================================================
assert_checkerror('usejava()', _('Not enough input arguments.'));
%=============================================================================
