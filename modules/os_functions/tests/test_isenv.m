%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
name = ['NELSON_TEST_ISENV_', createGUID()];
%=============================================================================
assert_isfalse(isenv(name));
%=============================================================================
setenv(name, 'present');
assert_istrue(isenv(name));
%=============================================================================
% a variable defined with an empty value still exists
setenv(name, '');
assert_istrue(isenv(name));
%=============================================================================
setenv(name);
assert_isfalse(isenv(name));
%=============================================================================
% logical class of the result
r = isenv('A_VARIABLE_THAT_DOES_NOT_EXIST');
assert_istrue(islogical(r));
assert_isequal(r, false);
%=============================================================================
% string array and cell array inputs
setenv(name, 'present');
r = isenv([string(name), "A_VARIABLE_THAT_DOES_NOT_EXIST"]);
assert_isequal(r, [true, false]);
r = isenv({name; 'A_VARIABLE_THAT_DOES_NOT_EXIST'});
assert_isequal(r, [true; false]);
setenv(name);
%=============================================================================
assert_checkerror('isenv()', _('Not enough input arguments.'));
assert_checkerror('isenv(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
%=============================================================================
