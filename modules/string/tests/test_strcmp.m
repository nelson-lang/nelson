%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.isequal(nargin('strcmp'), 2);
asserts.isequal(nargout('strcmp'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once', 'upon'; 'a', 'time'};
res = strcmp(c1, c2);
ref = [false, false; false, true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(c1, c1);
asserts.isequal(res, true);
%=============================================================================
res = strcmp(c2, c2);
ref = [true, true; true, true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(c2, 1);
ref = [false, false; false, false];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({}, []);
ref = logical([]);
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({}, {});
ref = logical([]);
asserts.isequal(res, ref);
%=============================================================================
res = strcmp('', 'test');
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(['ab'; 'cd'], 'abcd');
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(['ab'; 'cd'], 'acbd');
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(['a', 'b', 'c', 'd'], ['a'; 'b'; 'c'; 'd']);
ref = false;
asserts.isequal(res, ref);
%=============================================================================
A = {'abc', 'def', ['abc'; 'def']};
B = {['abc'; 'def']};
res = strcmp(A, B);
ref = [false false true];
asserts.isequal(res, ref);
%=============================================================================
A = {'abc', 'def', ['abc'; 'def']};
B = {['abc'; 'def'], ['a'; 'b'; 'c'], ['abc'; 'def']};
res = strcmp(A, B);
ref = [false false true];
asserts.isequal(res, ref);
%=============================================================================
asserts.checkerror('strcmpi()', _('Not enough input arguments.'));
%=============================================================================
res = strcmpi('teStStRiNg', 'TESTSTRING');
ref = true;
asserts.isequal(res, ref);
%=============================================================================
res = strcmpi('teStStRiNg', 'teststring');
ref = true;
asserts.isequal(res, ref);
%=============================================================================
res = strcmpi('teststring', 'teststrang');
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp('test', 'test');
ref = true;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp('test1', 'test2');
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({}, 'test');
ref = [];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({}, {'test'});
ref = [];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2', 'test1'}, {'test1'});
ref = [true false true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2'; 'test1', 'test3'}, 'test1');
ref = [true, false; true, false];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2', 'test1'}, 'test1');
ref = [true false true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({'test1', 'test2'; 'test1', 'test3'}, {'test2', 'test2'; 'test1', 'test1'});
ref = [false, true; true, false];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({'test1'}, {'test2', 'test2'; 'test1', 'test1'});
ref = [false, false; true, true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp('test1', {'test2', 'test2'; 'test1', 'test1'});
ref = [false, false; true, true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({{'test'}}, 'test');
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({{'test'}}, {'test'});
ref = false;
asserts.isequal(res, ref);
%=============================================================================
res = strcmp({65, {}; 'test1', 2i - 8}, {'A', {}; 'test1', 2i - 8});
ref = [false, false; true, false];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp('', '');
ref = true;
asserts.isequal(res, ref);
%=============================================================================
cmd = 'res = strcmp([''ab''; ''cd''], [''ab'';''c''])';
asserts.checkerror(cmd, _('Dimensions of arrays being concatenated are not consistent.'));
%=============================================================================
c1 = 'time';
c2 = ["Once", "upon"; "a", "time"];
res = strcmp(c1, c2);
ref = [false, false; false, true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(c1, c1);
asserts.isequal(res, true);
%=============================================================================
res = strcmp(c2, c2);
ref = [true, true; true, true];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(c2, 1);
ref = [false, false; false, false];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp(string([]), 3);
ref = [];
asserts.isequal(res, ref);
%=============================================================================
res = strcmp('', "test");
ref = false;
asserts.isequal(res, ref);
%=============================================================================
cmd = 'res = strcmp(["a", "b", "c", "d"], ["a"; "b"; "c"; "d"]);';
asserts.checkerror(cmd, _('Inputs must be the same size or either one can be a scalar.'));
%=============================================================================
A = ["abc", "def"; NaN, "def"];
B = 'def';
res = strcmp(A, B);
ref = [false, true; false, true];
asserts.isequal(res, ref);
%=============================================================================
A = ["abc", "def"; NaN, "def"];
B = {'abc'; 'def'};
asserts.checkerror('res = strcmp(A, B)', _('Inputs must be the same size or either one can be a scalar.'));
%=============================================================================
c1 = 'time';
c2 = {"Once", "upon"; "a", "time"};
res = strcmp(c1, c2);
REF = [false, false; false, false];
asserts.isequal(res, REF);
%=============================================================================
c1 = 'time';
c2 = ["Once", "upon"; "a", "time"];
res = strcmp(c1, c2);
REF = [false, false; false, true];
asserts.isequal(res, REF);
%=============================================================================
c1 = ["time"; "upon"];
c2 = ["Once", "upon"; "a", "time"];
asserts.checkerror('res = strcmp(c1, c2)', _('Inputs must be the same size or either one can be a scalar.'));
%=============================================================================
c1 = {'time'};
c2 = "time";
res = strcmp(c1, c2);
asserts.istrue(res);
%=============================================================================
c1 = {'time'};
c2 = 'time';
res = strcmp(c1, c2);
asserts.istrue(res);
%=============================================================================
R = strcmp({'a'}, ["a"]);
asserts.istrue(R);
%=============================================================================
R = strcmp({'a'; 'b'; 'd'; 'd'}, ["a"; "b"; "c"; "d"]);
REF = [true; true; false; true];
asserts.isequal(R, REF);
%=============================================================================
names = {'Alpha'; 'Beta'; 'EnableFeasibilityMode'; 'Gamma'};
query = 'EnableFeasibilityMode';
matches = false(size(names));
for k = 1:length(names)
  name = lower(names{k});
  matches(k) = strncmp(name, lower(query), length(query));
end
asserts.isequal(matches, [false; false; true; false]);
asserts.isequal(strncmp('EnableFeasibilityMode', 'enablefeasibilitymode', 21), false);
asserts.isequal(strncmpi('EnableFeasibilityMode', 'enablefeasibilitymode', 21), true);
%=============================================================================
res = strcmp("heatmap", {'heatmap', 'box'});
asserts.isequal(res, [true, false]);
%=============================================================================
res = strcmp({'heatmap', 'box'}, "heatmap");
asserts.isequal(res, [true, false]);
%=============================================================================
res = strcmp("time", {'Once', 'upon'; 'a', 'time'});
asserts.isequal(res, [false, false; false, true]);
%=============================================================================
res = strcmp({'heatmap'}, ["heatmap", "box"]);
asserts.isequal(res, [true, false]);
%=============================================================================
res = strcmp(["heatmap", "box"], {'box'});
asserts.isequal(res, [false, true]);
%=============================================================================
res = strcmpi("HEATMAP", {'heatmap', 'box'});
asserts.isequal(res, [true, false]);
%=============================================================================
