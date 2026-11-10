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
t3 = rand(2, 3, 4);
ab = 1;
savemat('s.mat', 'x', 't3', 'ab', '-v7.3');
m = matfile('s.mat');
%=============================================================================
asserts.isequal(size(m, 'x'), [4 4]);
asserts.isequal(size(m, 't3'), [2 3 4]);
asserts.isequal(size(m, "t3", 2), 3);
asserts.isequal(size(m, 'x', 3), 1);
[r, c] = size(m, 't3');
asserts.isequal([r, c], [2 12]);
[r, c, p, q] = size(m, 't3');
asserts.isequal([r, c, p, q], [2 3 4 1]);
asserts.isequal(size(m), [1 1]);
asserts.checkerror('size(m, ''nope'')', ...
  ['No variable ''nope'' in ''', fullfile(folder, 's.mat'), '''.']);
asserts.checkerror('size(m, ''x'', 0)', _('input must be positive.'));
%=============================================================================
asserts.isequal(who(m), {'ab'; 't3'; 'x'});
asserts.isequal(who(m, 'x'), {'x'});
asserts.isequal(who(m, 't*'), {'t3'});
asserts.isequal(who(m, '-regexp', '^a'), {'ab'});
txt = evalc('who(m)');
asserts.istrue(contains(txt, 'Your variables are:'));
%=============================================================================
w = whos(m);
asserts.isequal(size(w), [3 1]);
asserts.isequal({w.name}, {'ab', 't3', 'x'});
w = whos(m, 'x');
asserts.isequal(w.size, [4 4]);
asserts.isequal(w.class, 'double');
asserts.isequal(w.bytes, 128);
txt = evalc('whos(m)');
asserts.istrue(contains(txt, 'Name'));
asserts.istrue(contains(txt, 't3'));
%=============================================================================
% a file not created yet has no variables
n = matfile('later.mat');
asserts.isequal(who(n), {});
w = whos(n);
asserts.isequal(size(w), [0 1]);
asserts.istrue(isstruct(w));
asserts.checkerror('size(n, ''a'')', ...
  ['Variable ''a'' cannot be read: file ''', fullfile(folder, 'later.mat'), ''' not found.']);
%=============================================================================
