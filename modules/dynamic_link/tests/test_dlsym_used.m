%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_ref = [modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()];
lib = dlopen(path_ref);
sym = dlsym(lib, 'multiplicationDoubleByReference', 'libpointer', {'doublePtr'});
used = dlsym_used();
asserts.istrue(any(used == sym));
dlclose(lib);
%=============================================================================
