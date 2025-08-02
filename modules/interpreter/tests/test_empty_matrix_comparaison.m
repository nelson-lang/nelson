%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
a = [];
assert_istrue(isempty(a));
assert_isequal(size(a), [0 0]);
R = a == [];
assert_istrue(isempty(R));
assert_isequal(size(R), [0 0]);
assert_isequal(class(a == []), 'logical');
R = (1 == []);
assert_istrue(isempty(R));
assert_isequal(size(R), [0 0]);
assert_isequal(class(1 == []), 'logical');
%=============================================================================
