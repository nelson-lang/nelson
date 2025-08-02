%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('strlength'), 1);
assert_isequal(nargout('strlength'), 1);
%=============================================================================
c = 'Hello, Mad World';
res = strlength(c);
ref = 16;
assert_isequal(res, ref);
%=============================================================================
c = {'Hello, Mad World'};
res = strlength(c);
ref = 16;
assert_isequal(res, ref);
%=============================================================================
c = {''};
res = strlength(c);
ref = 0;
assert_isequal(res, ref);
%=============================================================================
c = '';
res = strlength(c);
ref = 0;
assert_isequal(res, ref);
%=============================================================================
c = {};
res = strlength(c);
ref = [];
assert_isequal(res, ref);
%=============================================================================
c = {'abs.docx','data.gz';'code.nlf','results.nlf'};
res = strlength(c);
ref = [8 7; 8 11];
assert_isequal(res, ref);
%=============================================================================
cmd = 'res = strlength(1)';
assert_checkerror(cmd, _('Wrong type for argument #1: string or cell expected.'));
%=============================================================================
B = ["Nel", NaN, "son"; "is", "open", "source"];
R = strlength(B);
REF = [3   NaN     3;    2     4     6];
assert_isequal(R, REF);
%=============================================================================
