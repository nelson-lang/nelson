%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
lib = dlopen([modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()]);
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarUInt8', 'uint8', {});
dlclose(lib);
%=============================================================================
assert_checkerror('r = dlcall(f);', _('dllib valid handle expected.'));
%=============================================================================
