%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a', 'b'}, 1);
assert_isequal(fieldnames(c), {'a'; 'b'})
assert_isequal(c(1).a, 1);
assert_isequal(c(2).a, 2);
assert_isequal(c(3).a, 3);
assert_isequal(c(4).a, 4);
assert_isequal(c(1).b, 5);
assert_isequal(c(2).b, 6);
assert_isequal(c(3).b, 7);
assert_isequal(c(4).b, 8);
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a'; 'b'; 'c'; 'd'}, 2);
assert_isequal(size(c), [2 1]);
assert_isequal(fieldnames(c), {'a'; 'b'; 'c'; 'd'});
assert_isequal(c(1).a, 1);
assert_isequal(c(1).b, 2);
assert_isequal(c(1).c, 3);
assert_isequal(c(1).d, 4);
assert_isequal(c(2).a, 5);
assert_isequal(c(2).b, 6);
assert_isequal(c(2).c, 7);
assert_isequal(c(2).d, 8);
%=============================================================================
c = cell2struct({}, [''; '']);
assert_isequal(fieldnames(c), cell(0, 1));
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a', 'b'; 'c', 'd'}, 2);
assert_isequal(size(c), [2, 1]);
assert_isequal(class(c), 'struct');
assert_isequal(c(1).a, 1);
assert_isequal(c(2).a, 5);
%=============================================================================
c = cell2struct({}, '');
assert_isequal(size(c), [0, 1]);
assert_isequal(class(c), 'struct');
%=============================================================================
c = cell2struct({}, {});
assert_isequal(size(c), [0, 1]);
assert_isequal(class(c), 'struct');
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a', 'b'});
assert_isequal(size(c), [4, 1]);
assert_isequal(class(c), 'struct');
assert_isequal(c(4).a, 4);
assert_isequal(c(4).b, 8);
%=============================================================================
clear s;
s.category = 'mouse';
s.height = 36.2;
s.name = 'bear';
c = struct2cell(s);
ref = {s.category; s.height; s.name};
assert_isequal(c, ref);
%=============================================================================
s = struct('name', {'Pierre', 'Anna', 'Roberta'}, 'age', {43, 21, 31});
c = struct2cell(s);
assert_isequal(c(:, :, 1), {'Pierre'; 43});
assert_isequal(c(:, :, 2), {'Anna'; 21});
assert_isequal(c(:, :, 3), {'Roberta'; 31});
%=============================================================================
S = cell2struct ({8; 6}, {'a'; 'b'});
assert_isequal(S.a, 8);
assert_isequal(S.b, 6);
%=============================================================================
v = {1,2,33;3,4,333};
f = {'a','b','x'};
S = cell2struct(v,f,2);
C = struct2cell(S);
ref = {1, 3; 2, 4; 33, 333};
assert_isequal(C, ref);
%=============================================================================
keys = {'one'; 'two'};
values = {1, 3; 2, 4};
s = cell2struct(values, keys);
assert_isequal(size(s),  [2, 1]);
%=============================================================================
c = {'bird', 'mouse', 65;  'nick', 'pakcer', 5};
fields = {'name', 'genus', 'height'};
dim = 2;
s = cell2struct(c, fields, dim);
C = struct2cell(s);
ref = {'bird', 'nick'; 'mouse', 'pakcer'; 65, 5};
assert_isequal(C,  ref);
%=============================================================================
S1 =  struct();
C1 = struct2cell(S1);
F1 = fieldnames(S1);
S11 = cell2struct(C1, F1);
assert_isequal(size(S11), [1 1]);
assert_isequal(size(fieldnames(S11)), [0 1]);
%=============================================================================
S5 = struct(ones(3,2,0));
C5 = struct2cell(S5);
F5 = fieldnames(S5);
S55 = cell2struct(C5, F5);
assert_isequal(size(S55), [3 2 0]);
assert_isequal(size(fieldnames(S55)), [0 1]);
%=============================================================================
S1 = struct();
C1 = struct2cell(S1);
F1 = fieldnames(S1);
S11 = cell2struct(C1, F1);
assert_isequal(class(S11), 'struct');
assert_isequal(size(fieldnames(S11)), [0 1]);
%=============================================================================
S2 = struct([]);
C2 = struct2cell(S2);
F2 = fieldnames(S2);
S22 = cell2struct(C2, F2);
assert_isequal(class(S22), 'struct');
%=============================================================================
S3 = struct(ones(0,1));
C3 = struct2cell(S3);
F3 = fieldnames(S3);
S33 = cell2struct(C3, F3);
assert_isequal(class(S33), 'struct');
%=============================================================================
S4 = struct(ones(1,0));
C4 = struct2cell(S4);
F4 = fieldnames(S4);
S44 = cell2struct(C4, F4);
assert_isequal(class(S44), 'struct');
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, ['abc';'def']);
assert_isequal(class(c), 'struct');
assert_isequal(size(c), [4 1]);
assert_isequal(fieldnames(c), {'abc'; 'def'});
assert_isequal(c(1).abc, 1);
assert_isequal(c(2).abc, 2);
assert_isequal(c(3).abc, 3);
assert_isequal(c(4).abc, 4);
assert_isequal(c(1).def, 5);
assert_isequal(c(2).def, 6);
assert_isequal(c(3).def, 7);
assert_isequal(c(4).def, 8);
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, ['a'; 'b']);
assert_isequal(class(c), 'struct');
assert_isequal(size(c), [4 1]);
assert_isequal(fieldnames(c), {'a'; 'b'});
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, [2, 3, 4]);';
assert_checkerror(cmd, _('A scalar expected.'));
%=============================================================================
assert_checkerror('cell2struct()', _('Wrong number of input arguments.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''})';
assert_checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a''; ''b''}, 2);';
assert_checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1 ,2 ,3, 4; 5, 6, 7, 8}, [''''; ''''])';
assert_checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''''}, 2)';
assert_checkerror(cmd, _('Field names must be valid.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8},{''a'', ''a''}, 1);';
assert_checkerror(cmd, _('Duplicated field detected.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b''}, 1.9);';
assert_checkerror(cmd, _('Expected a integer.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, 5);';
assert_checkerror(cmd, _('Not yet implemented with dim > 2'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, -1);';
assert_checkerror(cmd, _('Expected a positive integer scalar.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, 0)';
assert_checkerror(cmd, _('Dimension argument must be a positive integer scalar within indexing range.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {97, ''b'', ''c'', ''d''}, 2);';
assert_checkerror(cmd, _('A cell of string expected.'));
%=============================================================================
cmd = 'c = cell2struct({}, []);';
assert_checkerror(cmd, _('A cell or string array expected.'));
%=============================================================================
f = {'category','height','name'};
cmd = 's = cell2struct(c, f, 2);';
assert_checkerror(cmd, _('Wrong type for argument #1. cell expected.'));
%=============================================================================
cmd = 'S = cell2struct ({}, {''q''}, 3)';
assert_checkerror(cmd, _('Not yet implemented with dim > 2'));
%=============================================================================

