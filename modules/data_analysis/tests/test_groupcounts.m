%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[counts, groups] = groupcounts([1; 1; 2; 3; 3; 3]);
assert_isequal(counts, [2; 1; 3]);
assert_isequal(groups, [1; 2; 3]);
%=============================================================================
T = table({'a'; 'a'; 'b'}, [1; 2; 4], 'VariableNames', {'G', 'X'});
R = groupcounts(T, 'G');
assert_isequal(R.G, {'a'; 'b'});
assert_isequal(R.GroupCount, [2; 1]);
%=============================================================================
