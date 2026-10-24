%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Inside a classdef method whose name shadows a builtin, an unqualified call to
% that name with a non-class argument dispatches to the builtin (it must not
% recurse into the method).
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% isa(aDouble, 'double') inside the static isa method reaches the builtin
r = ClassdefBuiltinShadowMethod.isa(5, 'anything');
assert_isequal(r, false);
r = ClassdefBuiltinShadowMethod.isa(int8(5), 'anything');
assert_isequal(r, true);
%=============================================================================
% numel(aMatrix) inside the static numel method reaches the builtin
n = ClassdefBuiltinShadowMethod.numel([1 2 3; 4 5 6]);
assert_isequal(n, 1006);
%=============================================================================
