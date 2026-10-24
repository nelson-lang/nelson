%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('regress'), 3);
asserts.isequal(nargout('regress'), 5);
%=============================================================================
y = [1; 2.1; 2.9; 4.2; 5.1; 5.9];
X = [ones(6, 1), (1:6)'];
[b, bint, r, rint, stats] = regress(y, X);
asserts.isapprox(b, [0.053333333333333; 0.994285714285714], 1e-12);
asserts.isapprox(bint, [-0.295274524306209 0.401941190972875; ...
                       0.904771480016559 1.083799948554870], 1e-12);
asserts.isapprox(r, [-0.047619047619047; 0.058095238095238; -0.136190476190476; ...
                    0.169523809523810; 0.075238095238095; -0.119047619047619], 1e-12);
REFRINT = [-0.306024383877048 0.210786288638954; ...
           -0.256268421308617 0.372458897499094; ...
           -0.475085897563161 0.202704945182209; ...
           -0.169371611848876 0.508419230896495; ...
           -0.239125564165761 0.389601754641951; ...
           -0.377452955305619 0.139357717210382];
asserts.isapprox(rint, REFRINT, 1e-12);
asserts.isapprox(stats, [0.995811862734349 951.078534031414961 ...
                        0.000006586889601 0.018190476190476], 1e-12);
%=============================================================================
y2 = y;
y2(3) = NaN;
[b2, bint2, r2, rint2, stats2] = regress(y2, X, 0.01);
asserts.isapprox(b2, [0.097674418604652; 0.989534883720930], 1e-12);
asserts.istrue(isnan(r2(3)));
asserts.istrue(all(isnan(rint2(3, :))));
asserts.isapprox(r2([1 2 4 5 6]), [-0.087209302325582; 0.023255813953488; ...
                                   0.144186046511627; 0.054651162790696; ...
                                   -0.134883720930233], 1e-12);
asserts.isapprox(bint2, [-0.639494381340872 0.834843218550176; ...
                        0.807504023362717 1.171565744079144], 1e-12);
asserts.isapprox(stats2, [0.997033135266931 1008.168445475641 ...
                         0.000068647144096 0.016705426356589], 1e-12);
%=============================================================================
[b3, bint3, r3, rint3, stats3] = regress([1; 2], [1 1; 1 2]);
asserts.isapprox(b3, [0; 1], 1e-12);
asserts.istrue(all(isnan(bint3(:))));
asserts.istrue(all(isnan(rint3(:))));
asserts.istrue(isnan(stats3(2)));
%=============================================================================
asserts.checkerror('regress()', _('Not enough input arguments.'));
asserts.checkerror('regress({1}, [1])', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('regress([1], {1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('regress([1; 2], [1 2])', sprintf(_('%s must have the same number of rows.'), 'Y and X'));
asserts.checkerror('regress([1; 2], [1; 2], 0)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('regress([1; 2], [1; 2], 1)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
%=============================================================================
