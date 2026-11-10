%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(allunique([1 2 3]));
assert_isfalse(allunique([1 2 2]));
assert_istrue(allunique([1 NaN NaN]));
assert_istrue(allunique([]));
assert_istrue(allunique({'a', 'b'}));
assert_isfalse(allunique(["a" "b" "a"]));
assert_isfalse(allunique([1 2; 1 2], 'rows'));
assert_istrue(allunique([1 2; 2 1], 'rows'));
%=============================================================================
