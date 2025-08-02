%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('startsWith'), -3);
assert_isequal(nargout('startsWith'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once','upon'; 'a','time'};
res = startsWith(c1, c2);
ref = true;
assert_isequal(res, ref);
%=============================================================================
res = startsWith(c2, c1);
ref = [false, false; false, true];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','code.m','results.nlf'};
c2 = {'abs','data'};
res = startsWith(c1, c2);
ref = [true, true, false, false];
assert_isequal(res, ref);
%=============================================================================
c1 = {'Data.docx','data.gz','code.m','AbS.nlf'};
c2 = {'abs','data'};
res = startsWith(c1, c2, 'IgnoreCase', true);
ref = [true, true, false, true];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','code.m','results.nlf'};
c2 = {1};
assert_checkerror('res = startsWith(c1, c2);', _('char vector or cell of strings expected.'));
%=============================================================================
c1 = "time";
c2 = ["Once", "upon"; "a", "time"];
res = startsWith(c1, c2);
ref = true;
assert_isequal(res, ref);
%=============================================================================
c1 = ["Data.docx", "data.gz"; "code.m", "AbS.nlf"];
c2 = ["abs", "data"];
res = startsWith(c1, c2, 'IgnoreCase', true);
ref = [true, true; false, true];
assert_isequal(res, ref);
%=============================================================================
