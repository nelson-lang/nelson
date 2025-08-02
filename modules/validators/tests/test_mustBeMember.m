%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeMember'), -2);
assert_isequal(nargout('mustBeMember'), 0);
%=============================================================================
mustBeMember(1, 1);
%=============================================================================
A = 'red';
B = {'yellow','red','blue'};
mustBeMember(A,B)
%=============================================================================
A = 'red';
B = {'yellow','green','blue'};
msg = _('Value must be member of the compared value.');
assert_checkerror('mustBeMember(A, B)', msg, 'Nelson:validators:mustBeMember');
%=============================================================================
