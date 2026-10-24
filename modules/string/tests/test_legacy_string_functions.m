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
asserts.isequal(findstr('abcdef', 'cd'), 3);
asserts.isequal(findstr('你好你好', '好'), [2 4]);
asserts.isequal(strmatch('ab', {'abc'; 'ac'; 'ab'}), [1; 3]);
asserts.isequal(strmatch('ab', {'abc'; 'ac'; 'ab'}, 'exact'), 3);
asserts.isequal(strvcat('a', '', 'bb'), ['a '; 'bb']);
[A, B] = strread('1 one 2 two', '%d %s');
asserts.isequal(A, [1; 2]);
asserts.isequal(B, {'one'; 'two'});
asserts.isequal(strread('0.41 8.24 3.57 6.24 9.27', '%3.1f %*1d'), [0.4; 8.2; 3.5; 6.2; 9.2]);
[A, B, C] = strread('Section 4, Page 7, Line 26', '%s %s %s', 'delimiter', ',');
asserts.isequal(A, {'Section 4'});
asserts.isequal(B, {'Page 7'});
asserts.isequal(C, {'Line 26'});
%=============================================================================
