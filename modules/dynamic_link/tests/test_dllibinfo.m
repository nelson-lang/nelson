%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('dllibinfo'), 1);
assert_isequal(nargout('dllibinfo'), 1);
%=============================================================================
lib = dlopen([modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()]);
%=============================================================================
c = dllibinfo(lib);
assert_istrue(iscell(c));
assert_istrue(any(contains(c, 'sumDoubleRef')));
%=============================================================================
