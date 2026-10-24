%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('@dllib/get'), 2);
asserts.isequal(nargout('@dllib/get'), 1);
%=============================================================================
path_ref = modulepath('dynamic_link', 'builtin');
lib = dlopen(path_ref);
path_lib = get(lib, 'Path');
asserts.istrue(startsWith(path_lib, path_ref));
%=============================================================================
path_lib = lib.Path;
asserts.istrue(startsWith(path_lib, path_ref));
%=============================================================================
asserts.checkerror('path_lib = get(lib, ''Pat2'');', sprintf(_('Wrong value for #%d argument.'), 2));
%=============================================================================
dlclose(lib);
%=============================================================================
