%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('blanks'), 1);
assert_isequal(nargout('blanks'), 1);
%=============================================================================
r = blanks(3);
REF = '   ';
assert_isequal(r, REF);
%=============================================================================
r = blanks(-3);
REF = char(zeros(1,0));
assert_isequal(r, REF);
%=============================================================================
msg = message('nelson:validators:mustBeScalar');
assert_checkerror('r = blanks([1 3]);', msg);
%=============================================================================
msg = message('nelson:validators:mustBeFinite');
assert_checkerror('r = blanks(NaN);', msg);
%=============================================================================
msg = message('nelson:validators:mustBeInteger');
assert_checkerror('r = blanks(2.3);', msg);
%=============================================================================
