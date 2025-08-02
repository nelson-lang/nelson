%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('profsave'), 2);
assert_isequal(nargout('profsave'), 0);
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'), 'path');
profile('on')
run('script_to_profile.m');
profile('off')
p = profile('info');
%=============================================================================
assert_isequal(fieldnames(p), {'FunctionName'; 'Filename'; 'LinePosition'; 'NumCalls';'TotalTime'; 'PerCall'});
R1.FunctionName = 'colon';
R1.Filename = [currentpath, '/script_to_profile.m'];
R1.LinePosition = 10;
R1.NumCalls = 1;
%=============================================================================
R2.FunctionName = 'mod';
R2.Filename = [currentpath, '/script_to_profile.m'];
R2.LinePosition = 11;
R2.NumCalls = 10;
%=============================================================================
R3.FunctionName = 'plus';
R3.Filename = [currentpath, '/script_to_profile.m'];
R3.LinePosition = 12;
R3.NumCalls = 5;
%=============================================================================
R4.FunctionName = 'subsasgn'
R4.Filename = [currentpath, '/script_to_profile.m'];
R4.LinePosition = 12;
R4.NumCalls = 5;
%=============================================================================
R5.FunctionName = 'minus'
R5.Filename = [currentpath, '/script_to_profile.m']
R5.LinePosition = 14;
R5.NumCalls = 5;
%=============================================================================
R6.FunctionName = 'subsasgn';
R6.Filename = [currentpath, '/script_to_profile.m']
R6.LinePosition = 14;
R6.NumCalls = 5;
%=============================================================================
assert_isequal(p(1).FunctionName, R1.FunctionName);
assert_isequal(p(2).FunctionName, R2.FunctionName);
assert_isequal(p(3).FunctionName, R3.FunctionName);
assert_isequal(p(4).FunctionName, R4.FunctionName);
assert_isequal(p(5).FunctionName, R5.FunctionName);
assert_isequal(p(6).FunctionName, R6.FunctionName);
%=============================================================================
assert_isequal(p(1).LinePosition, R1.LinePosition);
assert_isequal(p(2).LinePosition, R2.LinePosition);
assert_isequal(p(3).LinePosition, R3.LinePosition);
assert_isequal(p(4).LinePosition, R4.LinePosition);
assert_isequal(p(5).LinePosition, R5.LinePosition);
assert_isequal(p(6).LinePosition, R6.LinePosition);
%=============================================================================
assert_isequal(p(1).NumCalls, R1.NumCalls);
assert_isequal(p(2).NumCalls, R2.NumCalls);
assert_isequal(p(3).NumCalls, R3.NumCalls);
assert_isequal(p(4).NumCalls, R4.NumCalls);
assert_isequal(p(5).NumCalls, R5.NumCalls);
assert_isequal(p(6).NumCalls, R6.NumCalls);
%=============================================================================
assert_isequal(p(1).Filename, R1.Filename);
assert_isequal(p(2).Filename, R2.Filename);
assert_isequal(p(3).Filename, R3.Filename);
assert_isequal(p(4).Filename, R4.Filename);
assert_isequal(p(5).Filename, R5.Filename);
assert_isequal(p(6).Filename, R6.Filename);
%=============================================================================
profile('show');
%=============================================================================
profile('show', 'nfl');
profile('show', 'nfl', 3);
%=============================================================================
