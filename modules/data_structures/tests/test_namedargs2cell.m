%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('namedargs2cell'), 1);
assert_isequal(nargout('namedargs2cell'), 1);
%=============================================================================
S = struct();
R = namedargs2cell(S);
REF = cell(1, 0);
assert_isequal(R, REF);
%=============================================================================
S = struct();
S.CharacterEncoding = 'auto';
S.Timeout = 5;
S.Username = "";
S.logical = false;
R = namedargs2cell(S);
REF = { 'CharacterEncoding', 'auto', 'Timeout', 5, 'Username', "", 'logical', false};
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = namedargs2cell(1);', _('Wrong type for argument #1. struct expected.'));
%=============================================================================
