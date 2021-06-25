%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('strcmp'), 2);
assert_isequal(nargout('strcmp'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once','upon'; 'a','time'};
res = strcmp(c1, c2);
ref = [false, false; false, true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp(c1, c1);
assert_isequal(res, true);
%=============================================================================
res = strcmp(c2, c2);
ref = [true, true; true, true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp(c2, 1);
ref = [false, false; false, false];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({}, []);
ref = [];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({}, {});
ref = [];
assert_isequal(res, ref);
%=============================================================================
res = strcmp('', 'test');
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp(['ab'; 'cd'], 'abcd');
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp(['ab'; 'cd'], 'acbd');
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp(['a', 'b', 'c', 'd'], ['a'; 'b'; 'c'; 'd']);
ref = false;
assert_isequal(res, ref);
%=============================================================================
A = {'abc', 'def', ['abc'; 'def']};
B =  {['abc'; 'def']};
res = strcmp(A, B);
ref = [false false true];
assert_isequal(res, ref);
%=============================================================================
A = {'abc', 'def', ['abc'; 'def']};
B = {['abc'; 'def'], ['a'; 'b'; 'c'], ['abc'; 'def']};
res = strcmp(A, B);
ref = [false false true];
assert_isequal(res, ref);
%=============================================================================
assert_checkerror('strcmpi()', _('Wrong number of input arguments.'));
%=============================================================================
res = strcmpi('teStStRiNg','TESTSTRING');
ref = true;
assert_isequal(res, ref);
%=============================================================================
res = strcmpi('teStStRiNg','teststring');
ref = true;
assert_isequal(res, ref);
%=============================================================================
res = strcmpi('teststring','teststrang');
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp('test', 'test');
ref = true;
assert_isequal(res, ref);
%=============================================================================
res = strcmp('test1','test2');
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp({},'test');
ref = [];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({}, {'test'});
ref = [];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2', 'test1'}, {'test1'});
ref = [true false true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2'; 'test1', 'test3'}, 'test1');
ref = [true, false; true, false];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2', 'test1'}, 'test1');
ref = [true false true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({'test1','test2'; 'test1', 'test3'}, {'test2', 'test2'; 'test1', 'test1'});
ref = [false, true; true, false];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({'test1'}, {'test2', 'test2'; 'test1', 'test1'});
ref = [false, false; true, true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp('test1', {'test2', 'test2'; 'test1', 'test1'});
ref = [false, false; true, true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp({{'test'}}, 'test');
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp({{'test'}}, {'test'});
ref = false;
assert_isequal(res, ref);
%=============================================================================
res = strcmp({65, {}; 'test1', 2i-8}, {'A', {}; 'test1', 2i-8});
ref = [false, false; true, false];
assert_isequal(res, ref);
%=============================================================================
res = strcmp('', '');
ref = true;
assert_isequal(res, ref);
%=============================================================================
cmd = 'res = strcmp([''ab''; ''cd''], [''ab'';''c''])';
assert_checkerror(cmd, _('Dimensions concatenated are not consistent.'));
%=============================================================================
c1 = 'time';
c2 = ["Once","upon"; "a","time"];
res = strcmp(c1, c2);
ref = [false, false; false, true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp(c1, c1);
assert_isequal(res, true);
%=============================================================================
res = strcmp(c2, c2);
ref = [true, true; true, true];
assert_isequal(res, ref);
%=============================================================================
res = strcmp(c2, 1);
ref = [false, false; false, false];
assert_isequal(res, ref);
%=============================================================================
res = strcmp(string([]), 3);
ref = [];
assert_isequal(res, ref);
%=============================================================================
res = strcmp('', "test");
ref = false;
assert_isequal(res, ref);
%=============================================================================
cmd = 'res = strcmp(["a", "b", "c", "d"], ["a"; "b"; "c"; "d"]);';
assert_checkerror(cmd, _('Same size expected.'));
%=============================================================================
A = ["abc", "def"; NaN, "def"];
B =  'def';
res = strcmp(A, B);
ref = [false, true; false, true];
assert_isequal(res, ref);
%=============================================================================
A = ["abc", "def"; NaN, "def"];
B =  {'abc'; 'def'};
assert_checkerror('res = strcmp(A, B)', _('Same size or scalar expected.'));
%=============================================================================