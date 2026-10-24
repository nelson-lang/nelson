%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
name = ['NELSON_TEST_UNSETENV_', createGUID()];
%=============================================================================
setenv(name, 'present');
assert_istrue(isenv(name));
%=============================================================================
unsetenv(name);
assert_isfalse(isenv(name));
assert_isequal(getenv(name), '');
%=============================================================================
% removing a variable that does not exist has no effect
unsetenv(name);
assert_isfalse(isenv(name));
%=============================================================================
% string scalar input
setenv(name, 'present');
unsetenv(string(name));
assert_isfalse(isenv(name));
%=============================================================================
assert_checkerror('unsetenv()', _('Not enough input arguments.'));
assert_checkerror('unsetenv(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
assert_checkerror('unsetenv(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'), 'Nelson:unsetenv:invalidInputType');
%=============================================================================
