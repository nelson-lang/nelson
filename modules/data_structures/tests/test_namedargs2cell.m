%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('namedargs2cell'), 1);
asserts.isequal(nargout('namedargs2cell'), 1);
%=============================================================================
S = struct();
R = namedargs2cell(S);
REF = cell(1, 0);
asserts.isequal(R, REF);
%=============================================================================
S = struct();
S.CharacterEncoding = 'auto';
S.Timeout = 5;
S.Username = "";
S.logical = false;
R = namedargs2cell(S);
REF = {'CharacterEncoding', 'auto', 'Timeout', 5, 'Username', "", 'logical', false};
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('R = namedargs2cell(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'struct'));
%=============================================================================
asserts.isequal(namedargs2cell(struct()), cell(1, 0));
NamedArgs = namedargs2cell(struct('CharacterEncoding', 'auto', 'Timeout', 5));
asserts.isequal(NamedArgs, {'CharacterEncoding', 'auto', 'Timeout', 5});
asserts.checkerror('namedargs2cell(1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'struct'));
%=============================================================================
