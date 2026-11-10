%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(numunique([1 2 2 NaN NaN]), 4);
assert_isequal(numunique([]), 0);
assert_isequal(numunique({'a', 'b', 'a'}), 2);
assert_isequal(numunique(["a" "b" "a"]), 2);
assert_isequal(numunique([1 2; 1 2], 'rows'), 1);
assert_isequal(class(numunique(single([1 2]))), 'double');
assert_checkerror('numunique([3 1 3], ''stable'')', _('The only supported option is ''rows''.'));
%=============================================================================
