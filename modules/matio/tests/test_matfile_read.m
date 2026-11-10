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
x = magic(4);
t3 = reshape(1:24, 2, 3, 4);
z = complex(1:3, 4:6);
zs = complex(single([1 2]), single([3 4]));
sg = single([1 2; 3 4]);
i8 = int8([1 2; 3 4]);
u64 = uint64([1 2; 3 4]);
lg = logical([1 0 1; 0 1 1]);
ch = ['abc'; 'def'];
c = {1, 'a'};
st = struct('a', {1, 2});
sp = sparse([1 0; 0 2]);
e = [];
savemat('data.mat', 'x', 't3', 'z', 'zs', 'sg', 'i8', 'u64', 'lg', 'ch', 'c', 'st', 'sp', 'e', '-v7.3');
m = matfile('data.mat');
src = fullfile(folder, 'data.mat');
%=============================================================================
% whole variables
asserts.isequal(m.x, x);
asserts.isequal(m.t3, t3);
asserts.isequal(m.z, z);
asserts.isequal(m.st, st);
%=============================================================================
% blocks of every class give the same values as indexing in memory
asserts.isequal(m.x(2, 3), x(2, 3));
asserts.isequal(m.x(1:2, [1 3]), x(1:2, [1 3]));
asserts.isequal(m.x(:, 2), x(:, 2));
asserts.isequal(m.x(1:2:4, 2:3), x(1:2:4, 2:3));
asserts.isequal(m.x(1, 1, 1), x(1, 1));
asserts.isequal(m.t3(:, 2, 3), t3(:, 2, 3));
asserts.isequal(m.t3(2, 1:2:3, 2:4), t3(2, 1:2:3, 2:4));
asserts.isequal(m.t3(1, 5), t3(1, 5));
asserts.isequal(m.z(1, 2:3), z(1, 2:3));
asserts.isequal(m.zs(1, 2), zs(1, 2));
asserts.isequal(m.sg(2, :), sg(2, :));
asserts.isequal(m.i8(2, :), i8(2, :));
asserts.isequal(m.u64(1:2, 2), u64(1:2, 2));
asserts.isequal(m.lg(1, 2:3), lg(1, 2:3));
asserts.isequal(m.ch(2, 2:3), ch(2, 2:3));
asserts.isequal(m.c(1, 2), c(1, 2));
asserts.isequal(m.st(1, 2), st(1, 2));
asserts.isequal(m.sp(1:2, 2), sp(1:2, 2));
asserts.isequal(m.x(end, 1), x(end, 1));
%=============================================================================
% subscripts are equally spaced increasing ranges, one per dimension
msg = 'Indices of ''x'' must increase with a constant step.';
asserts.checkerror('m.x([1 2 4], 1)', msg);
asserts.checkerror('m.x(4:-1:1, 1)', msg);
asserts.checkerror('m.x([2 1], 1)', msg);
msg = '''x'' needs one index per dimension (2): linear indexing is not supported.';
asserts.checkerror('m.x(5)', msg);
asserts.checkerror('m.x(1:3)', msg);
asserts.checkerror('m.x(0, 1)', 'Indices must be positive integers.');
asserts.checkerror('m.x(1.5, 1)', 'Indices must be positive integers.');
asserts.checkerror('m.x(logical([1 0 1 0]), 1)', ...
  'Indices of ''x'' must be numeric, not logical.');
asserts.checkerror('m.x(5, 1)', _('Index exceeds array bounds.'));
asserts.checkerror('m.x(1, 1, 2)', _('Index exceeds array bounds.'));
asserts.checkerror('m.x(1, [])', 'Indices of ''x'' must not be empty.');
asserts.checkerror('m.e(:, 1)', ''':'' cannot index the empty variable ''e''.');
%=============================================================================
asserts.checkerror('m.nope', ['No variable ''nope'' in ''', src, '''.']);
asserts.checkerror('m.nope(1, 1)', ['No variable ''nope'' in ''', src, '''.']);
%=============================================================================
n = matfile('missing.mat');
asserts.checkerror('n.a', ['Variable ''a'' cannot be read: file ''', fullfile(folder, 'missing.mat'), ''' not found.']);
%=============================================================================
