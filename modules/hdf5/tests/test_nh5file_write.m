%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
previous = cd(folder);
cleanup = onCleanup(@() cd(previous));
%=============================================================================
m = nh5file('w.nh5');
%=============================================================================
% whole variables are added or replaced
m.x = magic(4);
m.x = magic(6);
asserts.isequal(size(m, 'x'), [6 6]);
m.x = magic(3);
asserts.isequal(m.x, magic(3));
m.b = 'hello';
m.cc = {1, 2};
asserts.isequal(sort(who(m)), {'b'; 'cc'; 'x'});
%=============================================================================
% blocks inside the variable
m.x(2, 2) = 100;
expected = magic(3);
expected(2, 2) = 100;
asserts.isequal(m.x, expected);
m.x(1:2:3, 3) = [7; 8];
expected(1:2:3, 3) = [7; 8];
asserts.isequal(m.x, expected);
m.x(1:2, 1) = 9;
expected(1:2, 1) = 9;
asserts.isequal(m.x, expected);
%=============================================================================
% new variables and growth fill with zeros of the assigned class
m.a(2, 3) = 5;
asserts.isequal(m.a, [0 0 0; 0 0 5]);
m.a(3, 1:2) = [7 8];
asserts.isequal(m.a, [0 0 0; 0 0 5; 7 8 0]);
m.n(2, 2) = int16(7);
asserts.isequal(m.n, int16([0 0; 0 7]));
m.q(1:2:5, 2) = [1; 2; 3];
asserts.isequal(m.q, [0 1; 0 0; 0 2; 0 0; 0 3]);
m.t(2, 2, 2) = 1;
asserts.isequal(size(m, 't'), [2 2 2]);
m.y(81:100, 81:100) = magic(20);
asserts.isequal(size(m, 'y'), [100 100]);
asserts.isequal(m.y(85:94, 85:94), magic(20)(5:14, 5:14));
%=============================================================================
% deletion rewrites the variable
m.a(:, 1) = [];
asserts.isequal(m.a, [0 0; 0 5; 8 0]);
%=============================================================================
% the right side converts to the class of the variable when it can
m.d = zeros(2, 3);
m.d(1, 1) = single(2);
asserts.isequal(class(m.d), 'double');
asserts.isequal(m.d(1, 1), 2);
m.i = int8(zeros(2, 2));
m.i(1, 2) = int16(3);
asserts.isequal(m.i, int8([0 3; 0 0]));
m.z = complex([1 2], [3 4]);
m.z(1, 1) = 5;
asserts.isequal(m.z, complex([5 2], [0 4]));
m.ch = 'abcd';
m.ch(1, 2) = 'Z';
asserts.isequal(m.ch, 'aZcd');
m.l = true(2, 2);
m.l(1, 1) = false;
asserts.isequal(m.l, logical([0 1; 1 1]));
m.cc(1, 1) = {'z'};
asserts.isequal(m.cc, {'z', 2});
m.st = struct('a', {1, 2});
m.st(1, 1) = struct('a', 9);
asserts.isequal([m.st.a], [9 2]);
%=============================================================================
msg = 'The value cannot be converted to the class of the variable in the file.';
asserts.checkerror('m.d(1, 2) = true;', msg);
asserts.checkerror('m.d(1, 3) = ''a'';', msg);
asserts.checkerror('m.d(1, 1) = 1i;', msg);
asserts.checkerror('m.i(1, 1) = 3;', msg);
asserts.checkerror('m.l(1, 1) = 5;', msg);
asserts.checkerror('m.ch(1, 2) = 66;', msg);
asserts.checkerror('m.a(1:2, 1:2) = [1 2 3];', ...
  _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
asserts.checkerror('m.a(2, 2, 2) = 3;', ...
  '''a'' has 2 dimensions in the file but 3 indices were given.');
m.sp = sparse([1 0; 0 2]);
asserts.checkerror('m.sp(1, 1) = 5;', ...
  'Part of a sparse variable cannot be written: write the whole variable.');
asserts.checkerror('m.newc(:, 1) = 1;', ...
  ''':'' cannot size a dimension of a new variable.');
asserts.checkerror('m.newl(3) = 1;', ...
  'New variable ''newl'' needs at least two indices: linear indexing is not supported.');
asserts.checkerror('m.a(5) = 1;', ...
  '''a'' needs one index per dimension (2): linear indexing is not supported.');
%=============================================================================
% read-only objects refuse every change
r = nh5file('w.nh5');
asserts.checkerror('r.x = 3;', ...
  '''x'' cannot be modified: Properties.Writable is false.');
asserts.checkerror('r.x(1, 1) = 3;', ...
  '''x'' cannot be modified: Properties.Writable is false.');
%=============================================================================
% the file stays a regular .nh5 file
data = loadnh5('w.nh5');
asserts.isequal(data.x, expected);
asserts.isequal(data.y(81:100, 81:100), magic(20));
asserts.isequal(data.n, int16([0 0; 0 7]));
%=============================================================================
