%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('dbstack'), -1);
assert_isequal(nargout('dbstack'), -1);
%=============================================================================
dbstack();
dbstack('-completenames');
%=============================================================================
[st, idx] = dbstack();
ref_st_1.file = 'test_dbstack.m';
ref_st_1.name = 'test_dbstack';
ref_st_1.line = 16;
assert_isequal(st(1), ref_st_1);
ref_st_2.file = '';
ref_st_2.name = 'run';
ref_st_2.line = zeros(0, 1);
assert_isequal(st(2), ref_st_2);
%=============================================================================
dbstack();
dbstack('-completenames');
%=============================================================================
addpath(fileparts(nfilename('fullpathext'), 'path'));
[st, idx] = fun_dbstack();
%=============================================================================
ref_st_3.file = '';
ref_st_3.name = 'run';
ref_st_3.line = zeros(0, 1);
assert_isequal(st(3), ref_st_3);
%=============================================================================
ref_st_2.file = 'test_dbstack.m';
ref_st_2.name = 'test_dbstack';
ref_st_2.line = 30;
assert_isequal(st(2), ref_st_2);
%=============================================================================
ref_st_1.file = 'fun_dbstack.m';
ref_st_1.name = 'fun_dbstack';
ref_st_1.line = 13;
assert_isequal(st(1), ref_st_1);
%=============================================================================
[st, idx] = dbstack(1);
ref_st_1.file = '';
ref_st_1.name = 'run';
ref_st_1.line = zeros(0, 1);
assert_isequal(st(1), ref_st_1);
%=============================================================================
