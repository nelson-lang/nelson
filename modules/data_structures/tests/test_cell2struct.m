%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a', 'b'}, 1);
asserts.isequal(fieldnames(c), {'a'; 'b'})
asserts.isequal(c(1).a, 1);
asserts.isequal(c(2).a, 2);
asserts.isequal(c(3).a, 3);
asserts.isequal(c(4).a, 4);
asserts.isequal(c(1).b, 5);
asserts.isequal(c(2).b, 6);
asserts.isequal(c(3).b, 7);
asserts.isequal(c(4).b, 8);
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a'; 'b'; 'c'; 'd'}, 2);
asserts.isequal(size(c), [2 1]);
asserts.isequal(fieldnames(c), {'a'; 'b'; 'c'; 'd'});
asserts.isequal(c(1).a, 1);
asserts.isequal(c(1).b, 2);
asserts.isequal(c(1).c, 3);
asserts.isequal(c(1).d, 4);
asserts.isequal(c(2).a, 5);
asserts.isequal(c(2).b, 6);
asserts.isequal(c(2).c, 7);
asserts.isequal(c(2).d, 8);
%=============================================================================
c = cell2struct({}, [''; '']);
asserts.isequal(fieldnames(c), cell(0, 1));
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a', 'b'; 'c', 'd'}, 2);
asserts.isequal(size(c), [2, 1]);
asserts.isequal(class(c), 'struct');
asserts.isequal(c(1).a, 1);
asserts.isequal(c(2).a, 5);
%=============================================================================
c = cell2struct({}, '');
asserts.isequal(size(c), [0, 1]);
asserts.isequal(class(c), 'struct');
%=============================================================================
c = cell2struct({}, {});
asserts.isequal(size(c), [0, 1]);
asserts.isequal(class(c), 'struct');
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a', 'b'});
asserts.isequal(size(c), [4, 1]);
asserts.isequal(class(c), 'struct');
asserts.isequal(c(4).a, 4);
asserts.isequal(c(4).b, 8);
%=============================================================================
clear s;
s.category = 'mouse';
s.height = 36.2;
s.name = 'bear';
c = struct2cell(s);
ref = {s.category; s.height; s.name};
asserts.isequal(c, ref);
%=============================================================================
s = struct('name', {'Pierre', 'Anna', 'Roberta'}, 'age', {43, 21, 31});
c = struct2cell(s);
asserts.isequal(c(:, :, 1), {'Pierre'; 43});
asserts.isequal(c(:, :, 2), {'Anna'; 21});
asserts.isequal(c(:, :, 3), {'Roberta'; 31});
%=============================================================================
S = cell2struct({8; 6}, {'a'; 'b'});
asserts.isequal(S.a, 8);
asserts.isequal(S.b, 6);
%=============================================================================
v = {1, 2, 33; 3, 4, 333};
f = {'a', 'b', 'x'};
S = cell2struct(v, f, 2);
C = struct2cell(S);
ref = {1, 3; 2, 4; 33, 333};
asserts.isequal(C, ref);
%=============================================================================
keys = {'one'; 'two'};
values = {1, 3; 2, 4};
s = cell2struct(values, keys);
asserts.isequal(size(s), [2, 1]);
%=============================================================================
c = {'bird', 'mouse', 65; 'nick', 'pakcer', 5};
fields = {'name', 'genus', 'height'};
dim = 2;
s = cell2struct(c, fields, dim);
C = struct2cell(s);
ref = {'bird', 'nick'; 'mouse', 'pakcer'; 65, 5};
asserts.isequal(C, ref);
%=============================================================================
S1 = struct();
C1 = struct2cell(S1);
F1 = fieldnames(S1);
S11 = cell2struct(C1, F1);
asserts.isequal(size(S11), [1 1]);
asserts.isequal(size(fieldnames(S11)), [0 1]);
%=============================================================================
S5 = struct(ones(3, 2, 0));
C5 = struct2cell(S5);
F5 = fieldnames(S5);
S55 = cell2struct(C5, F5);
asserts.isequal(size(S55), [3 2 0]);
asserts.isequal(size(fieldnames(S55)), [0 1]);
%=============================================================================
S1 = struct();
C1 = struct2cell(S1);
F1 = fieldnames(S1);
S11 = cell2struct(C1, F1);
asserts.isequal(class(S11), 'struct');
asserts.isequal(size(fieldnames(S11)), [0 1]);
%=============================================================================
S2 = struct([]);
C2 = struct2cell(S2);
F2 = fieldnames(S2);
S22 = cell2struct(C2, F2);
asserts.isequal(class(S22), 'struct');
%=============================================================================
S3 = struct(ones(0, 1));
C3 = struct2cell(S3);
F3 = fieldnames(S3);
S33 = cell2struct(C3, F3);
asserts.isequal(class(S33), 'struct');
%=============================================================================
S4 = struct(ones(1, 0));
C4 = struct2cell(S4);
F4 = fieldnames(S4);
S44 = cell2struct(C4, F4);
asserts.isequal(class(S44), 'struct');
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, ['abc'; 'def']);
asserts.isequal(class(c), 'struct');
asserts.isequal(size(c), [4 1]);
asserts.isequal(fieldnames(c), {'abc'; 'def'});
asserts.isequal(c(1).abc, 1);
asserts.isequal(c(2).abc, 2);
asserts.isequal(c(3).abc, 3);
asserts.isequal(c(4).abc, 4);
asserts.isequal(c(1).def, 5);
asserts.isequal(c(2).def, 6);
asserts.isequal(c(3).def, 7);
asserts.isequal(c(4).def, 8);
%=============================================================================
c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, ['a'; 'b']);
asserts.isequal(class(c), 'struct');
asserts.isequal(size(c), [4 1]);
asserts.isequal(fieldnames(c), {'a'; 'b'});
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, [2, 3, 4]);';
asserts.checkerror(cmd, _('A scalar expected.'));
%=============================================================================
asserts.checkerror('cell2struct()', _('Not enough input arguments.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''})';
asserts.checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a''; ''b''}, 2);';
asserts.checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1 ,2 ,3, 4; 5, 6, 7, 8}, [''''; ''''])';
asserts.checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''''}, 2)';
asserts.checkerror(cmd, _('Field names must be valid.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8},{''a'', ''a''}, 1);';
asserts.checkerror(cmd, _('Duplicated field detected.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b''}, 1.9);';
asserts.checkerror(cmd, _('A real integer value scalar expected.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, 5);';
asserts.checkerror(cmd, _('Number of field names must match number of fields in new structure.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, -1);';
asserts.checkerror(cmd, _('Expected a positive integer scalar.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {''a'', ''b'', ''c'', ''d''}, 0)';
asserts.checkerror(cmd, _('Dimension argument must be a positive integer scalar within indexing range.'));
%=============================================================================
cmd = 'c = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {97, ''b'', ''c'', ''d''}, 2);';
asserts.checkerror(cmd, _('A cell of string expected.'));
%=============================================================================
cmd = 'c = cell2struct({}, []);';
asserts.checkerror(cmd, _('A cell or string array expected.'));
%=============================================================================
f = {'category', 'height', 'name'};
cmd = 's = cell2struct(c, f, 2);';
asserts.checkerror(cmd, sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'cell'));
%=============================================================================
S = cell2struct({}, {'q'}, 3);
asserts.isequal(size(S), [0 0]);
asserts.isequal(fieldnames(S), {'q'});
%=============================================================================

StructArray = cell2struct({1, 2; 3, 4}, {'a', 'b'}, 1);
asserts.isequal(size(StructArray), [2 1]);
asserts.isequal(StructArray(2).a, 2);
asserts.isequal(StructArray(1).b, 3);
CellStructDim2 = cell2struct({1, 2, 3, 4; 5, 6, 7, 8}, {'a'; 'b'; 'c'; 'd'}, 2);
asserts.isequal(size(CellStructDim2), [2 1]);
asserts.isequal(fieldnames(CellStructDim2), {'a'; 'b'; 'c'; 'd'});
asserts.isequal(CellStructDim2(2).d, 8);
asserts.checkerror('cell2struct({1, 2}, {''a'', ''a''}, 2)', _('Duplicated field detected.'));
%=============================================================================
% N-D cells, dim beyond the cell dimensions, empty cells
C = num2cell(ones(2, 3, 2));
S = cell2struct(C, {'a'; 'b'}, 1);
asserts.isequal(size(S), [3 2]);
S = cell2struct(C, {'a'; 'b'; 'c'}, 2);
asserts.isequal(size(S), [2 2]);
S = cell2struct(C, {'a'; 'b'}, 3);
asserts.isequal(size(S), [2 3]);
C = num2cell(reshape(1:12, 2, 3, 2));
S = cell2struct(C, {'a'; 'b'}, 3);
asserts.isequal(S(2, 3).a, 6);
asserts.isequal(S(2, 3).b, 12);
S = cell2struct(C, {'a'; 'b'}, 1);
asserts.isequal(S(3, 2).a, 11);
asserts.isequal(S(3, 2).b, 12);
asserts.isequal(size(cell2struct({1, 2}, {'a'}, 3)), [1 2]);
asserts.isequal(cell2struct({1}, {'a'}, 3), struct('a', 1));
asserts.isequal(size(cell2struct(cell(0, 3), {}, 1)), [3 1]);
asserts.isequal(size(cell2struct(cell(0, 2), {'a'; 'b'}, 2)), [0 1]);
asserts.isequal(size(cell2struct(cell(2, 0), {'a'; 'b'}, 1)), [0 1]);
asserts.checkerror('cell2struct({}, {''a''}, 1)', _('Number of field names must match number of fields in new structure.'));
asserts.checkerror('cell2struct({1; 2}, {''1a''; ''b''}, 1)', _('Field names must be valid.'));
%=============================================================================
