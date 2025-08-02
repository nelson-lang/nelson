%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@dllib/get'), 2);
assert_isequal(nargout('@dllib/get'), 1);
%=============================================================================
path_ref = modulepath('dynamic_link', 'builtin');
lib = dlopen(path_ref);
path_lib = get(lib, 'Path');
assert_istrue(startsWith(path_lib, path_ref));
%=============================================================================
path_lib = lib.Path;
assert_istrue(startsWith(path_lib, path_ref));
%=============================================================================
assert_checkerror('path_lib = get(lib, ''Pat2'');', _('Wrong value for #2 argument.'));
%=============================================================================
dlclose(lib);
%=============================================================================
