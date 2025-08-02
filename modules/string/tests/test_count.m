%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('count'), -3);
assert_isequal(nargout('count'), 1);
%=============================================================================
res = count('nelson', 'n');
assert_isequal(res, 2);
%=============================================================================
c1 = 'time';
c2 = {'Once','upon'; 'a','time'};
res = count(c1, c2);
ref = 1;
assert_isequal(res, ref);
%=============================================================================
res = count(c2, c1);
ref = [0, 0; 0, 1];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','code.m','results.nlf'};
c2 = {'abs','data'};
res = count(c1, c2);
ref = [1, 1, 0, 0];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','ABS.m','DAta.nlf'};
c2 = {'abs','data'};
res = count(c1, c2, 'IgnoreCase', true);
ref = [1, 1, 1, 1];
assert_isequal(res, ref);
%=============================================================================
c1 = {'blue green red green blue red green';
'yellow red red green green red'};
res = count(c1, 'green');
ref = [3; 2];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','code.m','results.nlf'};
c2 = {1};
assert_checkerror('res = count(c1, c2);', _('char vector or cell of strings expected.'));
%=============================================================================
c1 = 'time';
c2 = ["Once", "upon"; "a", "time"];
res = count(c1, c2);
ref = 1;
assert_isequal(res, ref);
%=============================================================================
c1 = ["abs.docx", "data.gz"; "ABS.m", "DAta.nlf"];
c2 = ["abs", "data"];
res = count(c1, c2, 'IgnoreCase', true);
ref = [1, 1; 1, 1];
assert_isequal(res, ref);
%=============================================================================
