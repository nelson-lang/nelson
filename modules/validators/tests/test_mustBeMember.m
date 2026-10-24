%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeMember'), -2);
asserts.isequal(nargout('mustBeMember'), 0);
%=============================================================================
mustBeMember(1, 1);
%=============================================================================
A = 'red';
B = {'yellow', 'red', 'blue'};
mustBeMember(A, B)
%=============================================================================
A = 'red';
B = {'yellow', 'green', 'blue'};
msg = sprintf(_('%s must be member of the compared value.'), 'Value');
asserts.checkerror('mustBeMember(A, B)', msg, 'Nelson:validators:mustBeMember');
%=============================================================================
% every element must be a member of the set; empty values are accepted
mustBeMember([1 2 2], [1 2]);
mustBeMember([], [1 2]);
mustBeMember({'a', 'b'}, {'a', 'b', 'c'});
msg = sprintf(_('%s must be member of the compared value.'), 'Value');
asserts.checkerror('mustBeMember([1 5], [1 2])', msg, 'Nelson:validators:mustBeMember');
asserts.checkerror('mustBeMember(["a", "z"], ["a", "b"])', msg, 'Nelson:validators:mustBeMember');
%=============================================================================
