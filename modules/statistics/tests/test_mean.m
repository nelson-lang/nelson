%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('mean'), 4)
asserts.isequal(nargout('mean'), 1)
%=============================================================================
R = mean([complex(1, NaN), complex(1, 2)], 'omitnan');
asserts.isequal(R, complex(1, 2));
%=============================================================================
R = mean([complex(1, NaN), complex(1, 2)], 'includenan');
asserts.isequal(R, complex(1, NaN));
%=============================================================================
R = mean([complex(1, NaN), complex(NaN, 2)], 'omitnan');
asserts.isequal(R, NaN);
%=============================================================================
R = mean([complex(1, NaN), complex(NaN, 2)], 'includenan');
asserts.isequal(R, complex(NaN, NaN));
%=============================================================================
R = mean([1 2], 1);
asserts.isequal(R, [1 2]);
%=============================================================================
R = mean([1 2], 2);
asserts.isequal(R, 1.5);
%=============================================================================
R = mean([1 2], 3);
asserts.isequal(R, [1 2]);
%=============================================================================
R = mean(single([1 0 1 1]));
asserts.isequal(R, single(0.75));
%=============================================================================
R = mean(logical([1 0 1 1]));
asserts.isequal(R, 0.75)
%=============================================================================
R = mean([4 4 2]);
asserts.isequal(R, 3 + 1 / 3);
%=============================================================================
R = mean([2 8]);
asserts.isequal(R, 5);
%=============================================================================
R = mean(repmat(0.1, 1, 10000));
asserts.isapprox(R, 0.1000, 1e-4);
%=============================================================================
X = -100:100;
R = mean(X);
asserts.isequal(R, 0);
%=============================================================================
Y = X';
R = mean(Y);
asserts.isequal(R, 0);
%=============================================================================
Z = [Y, Y + 100];
R = mean(Z);
asserts.isequal(R, [0 100]);
%=============================================================================
M = [0 1 1; 2 3 2; 1 3 2; 4 2 2];
R = mean(M);
REF = [1.7500 2.2500 1.7500];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = mean(M, 'all');
REF = 1.9167;
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = mean(M, 1);
REF = [1.7500 2.2500 1.7500];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = mean(M, 2);
REF = [0.6667; 2.3333; 2.0000; 2.6667];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = mean(M, 3);
asserts.isequal(R, M);
%=============================================================================
M = [1:24];
M = reshape(M, 3, 2, 4);
R = mean(M, 1);
asserts.isequal(R(:, :, 1), [2, 5]);
asserts.isequal(R(:, :, 2), [8, 11]);
asserts.isequal(R(:, :, 3), [14, 17]);
asserts.isequal(R(:, :, 4), [20, 23]);
%=============================================================================
M = [1:24];
M = reshape(M, 3, 2, 4);
R = mean(M, 'all');
asserts.isequal(R, 12.5);
%=============================================================================
A = single(ones(10, 1));
R = mean(A, 'native');
asserts.isequal(R, single(1));
%=============================================================================
A = single(ones(10, 1));
R = mean(A, 'default');
asserts.isequal(R, single(1));
%=============================================================================
A = single(ones(10, 1));
R = mean(A, 'double');
asserts.isequal(R, double(1));
%=============================================================================
A = [1 0 0 1 NaN 1 NaN 0];
R = mean(A, 'omitnan');
asserts.isequal(R, 0.5);
%=============================================================================
A = [1 0 0 1 NaN 1 NaN 0];
R = mean(A, 'includenan');
asserts.isequal(R, NaN);
%=============================================================================
asserts.isequal(mean([1 NaN 3], 'omitnan'), 2);
asserts.isequal(size(mean(zeros(0, 3), 1)), [1 3]);
%=============================================================================
% mean of a complex array must equal sum(A, dim) ./ size(A, dim) (double and single, all dims, N-D)
Cd = reshape((1:24) + 1i * (24:-1:1), 3, 2, 4);
for d = [1 2 3]
  asserts.isapprox(mean(Cd, d), sum(Cd, d) ./ size(Cd, d), 1e-10);
end
asserts.isapprox(mean(Cd), sum(Cd, 1) ./ size(Cd, 1), 1e-10);
asserts.isapprox(mean(Cd, 'all'), sum(Cd(:)) ./ numel(Cd), 1e-10);
%=============================================================================
Cs = single(Cd);
for d = [1 2 3]
  asserts.isapprox(mean(Cs, d), sum(Cs, d) ./ single(size(Cs, d)), single(1e-4));
end
asserts.isequal(class(mean(Cs, 2)), 'single');
%=============================================================================
% column-vector complex mean along dim 1
Cv = (1:6)' + 1i * (6:-1:1)';
asserts.isapprox(mean(Cv, 1), sum(Cv, 1) ./ size(Cv, 1), 1e-10);
%=============================================================================
% vector of dimensions (vecdim)
A = reshape(1:24, 2, 3, 4);
assert_isequal(mean(A, [1 2]), reshape([3.5 9.5 15.5 21.5], 1, 1, 4));
assert_isequal(mean(A, [1 3]), [10.5 12.5 14.5]);
assert_isequal(mean(A, [1 2 3]), 12.5);
assert_isequal(mean([1 2 3; 4 5 6], [1 2]), 3.5);
assert_checkerror('mean(A, [1 2], ''omitnan'')', 'The ''omitnan'' option is not supported with a vector of dimensions.');
%=============================================================================
% an unknown option name
assert_checkerror('mean([1 2 3], ''foo'')', sprintf(_('Invalid %s argument.'), 'second'), 'Nelson:statistics:invalidValue:invalidArgument');
assert_checkerror('mean([1 2 3], 1, ''foo'')', sprintf(_('Invalid %s argument.'), 'third'), 'Nelson:statistics:invalidValue:invalidArgument');
%=============================================================================
