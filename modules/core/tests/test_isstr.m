%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isstr'), 1);
assert_isequal(nargout('isstr'), 1);
%=============================================================================
% isstr is a deprecated alias for ischar: true only for char arrays.
assert_istrue(isstr('abc'));
assert_istrue(isstr(''));
assert_istrue(isstr(['ab'; 'cd']));
%=============================================================================
assert_isfalse(isstr(42));
assert_isfalse(isstr(true));
assert_isfalse(isstr({'abc'}));
%=============================================================================
% string is not char, so isstr returns false (as ischar does).
assert_isfalse(isstr("x"));
%=============================================================================
% same result as ischar on every input.
assert_isequal(isstr('abc'), ischar('abc'));
assert_isequal(isstr(42), ischar(42));
assert_isequal(isstr("x"), ischar("x"));
%=============================================================================
assert_checkerror('isstr()', _('Not enough input arguments.'));
%=============================================================================
