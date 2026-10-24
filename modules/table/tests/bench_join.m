%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Single key, every left row matching exactly one right row.
N = 4000;
L = table((1:N)', rand(N, 1), 'VariableNames', {'k', 'a'});
R = table((1:N)', rand(N, 1), 'VariableNames', {'k', 'b'});
tic();
J = innerjoin(L, R, 'Keys', 'k');
toc();
asserts.isequal(height(J), N);
%=============================================================================
% Two keys with unmatched rows on both sides: left tuples (i, i) and right
% tuples (i + h, i + h) overlap on half, so h rows match, h are left-only and
% h are right-only.
N = 4000;
h = floor(N / 2);
L = table((1:N)', (1:N)', rand(N, 1), 'VariableNames', {'k1', 'k2', 'a'});
R = table(((1:N)' + h), ((1:N)' + h), rand(N, 1), 'VariableNames', {'k1', 'k2', 'b'});
tic();
Ji = innerjoin(L, R, 'Keys', {'k1', 'k2'});
Jf = outerjoin(L, R, 'Keys', {'k1', 'k2'});
Jl = outerjoin(L, R, 'Keys', {'k1', 'k2'}, 'Type', 'left');
toc();
asserts.isequal(height(Ji), h);
asserts.isequal(height(Jf), 3 * h);
asserts.isequal(height(Jl), N);
%=============================================================================
% Duplicate keys on both sides: each distinct key appears d times on the left
% and d times on the right, so the inner join expands many-to-many to d * d
% rows per key.
N = 4000;
d = 4;
G = N / d;
L = table(repmat((1:G)', d, 1), rand(N, 1), 'VariableNames', {'k', 'a'});
R = table(repmat((1:G)', d, 1), rand(N, 1), 'VariableNames', {'k', 'b'});
tic();
J = innerjoin(L, R, 'Keys', 'k');
toc();
asserts.isequal(height(J), G * d * d);
%=============================================================================
