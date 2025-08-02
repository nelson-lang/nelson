%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- NO USER MODULES -->
%=============================================================================
path_1 = modulepath('dynamic_link', 'builtin');
path_2 = modulepath('core', 'builtin');
assert_isequal(size(dllib_used()), [0 0]);
lib1 = dlopen(path_1);
assert_isequal(size(dllib_used()), [1 1]);
lib2 = dlopen(path_2);
assert_isequal(size(dllib_used()), [1 2]);
assert_isfalse(dllibisloaded(['nlscore2', getdynlibext()]));
assert_isequal(size(dllib_used()), [1 2]);
assert_istrue(dllibisloaded(path_1));
assert_isequal(size(dllib_used()), [1 2]);
assert_istrue(dllibisloaded(path_2));
assert_isequal(size(dllib_used()), [1 2]);
[t, lib] = dllibisloaded(path_1);
assert_isequal(lib1, lib);
assert_isequal(size(dllib_used()), [1 2]);
assert_checkerror('dllibisloaded(3);', _('Unable to convert supplied object to a string.'));
%=============================================================================
