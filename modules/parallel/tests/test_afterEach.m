%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
pool = backgroundPool();
%=============================================================================
clear f
f1 = str2func('@(x) x * 10');
f2 = str2func('@(x) x / 10');
for idx = 1:100
    f(idx) = parfeval(pool, f1, 1, idx);
end
c = afterEach(f, f2, 1);
R = fetchOutputs(c);
REF = [1:100]';
assert_isequal(length(R), length(REF));
assert_isequal(R, REF)
%=============================================================================
fevalqueue = pool.FevalQueue;
cancelAll(fevalqueue);
%=============================================================================
clear f
fn1 = str2func('@(x) [x:-1:1]''');
fn2 = str2func('max');
for idx= 1:10
    f(idx) = parfeval(pool, fn1, 1, 1000);
end
C = afterEach(f, fn2, 2);
[R, I] = fetchOutputs(C);
REF = [ones(10, 1) * 1000];
assert_isequal(length(R), length(REF));
assert_isequal(R, REF);
assert_isequal(I, ones(10, 1));
%=============================================================================
fevalqueue = pool.FevalQueue;
cancelAll(fevalqueue);
%=============================================================================
clear f
fn1 = str2func('@(x) [x:-1:1]');
fn2 = str2func('max');
for idx= 1:10
    f(idx) = parfeval(pool, fn1, 1, 1000);
end
C = afterEach(f, fn2, 2);
[R, I] = fetchOutputs(C);
REF = [ones(10, 1) * 1000];
assert_isequal(length(R), length(REF));
assert_isequal(R, REF);
assert_isequal(I, ones(10, 1));
%=============================================================================
fevalqueue = pool.FevalQueue;
cancelAll(fevalqueue);
%=============================================================================
