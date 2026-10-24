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
asserts.isequal(nargin('wblfit'), 5);
asserts.isequal(nargout('wblfit'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[PHAT, PCI] = wblfit(x);
asserts.isapprox(PHAT, [3.499523978509226 1.254115126547431], 1e-4);
asserts.isapprox(PCI, [1.783517048721508 0.6649907100699163;
                      6.866583128509990 2.365152966527481], 1e-4);
%=============================================================================
[PHAT, PCI] = wblfit(x, 0.01);
asserts.isapprox(PHAT, [3.499523978509226 1.254115126547431], 1e-4);
asserts.isapprox(PCI, [1.443092919067591 0.5448040723689486;
                      8.486402998965470 2.886918124154465], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[PHAT, PCI] = wblfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, [4.254789570500730 1.060085049217854], 1e-4);
asserts.isapprox(PCI, [1.999623603103103 0.5489073266910343;
                      9.053320965579921 2.047304266003660], 1e-4);
%=============================================================================
opts = statset('MaxIter', 120, 'TolX', 1e-4);
[PHAT2, PCI2] = wblfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, [3.499523978509226 1.254115126547431], 1e-4);
asserts.isapprox(PCI2, [1.783517048721508 0.6649907100699163;
                       6.866583128509990 2.365152966527481], 1e-4);
%=============================================================================
xm = [0.5 1; 1 2; 2 3; 3 4; 5 7];
[PHATM, PCIM] = wblfit(xm);
asserts.isequal(size(PHATM), [2 2]);
asserts.isapprox(PHATM, [2.546813461412184 1.460964597843973;
                        3.836440034457059 1.745688644345655], 1e-4);
asserts.isequal(size(PCIM), [2 2 2]);
asserts.isapprox(PCIM(:, :, 1), [1.351567517127800 0.7260825585588124;
                                4.799063846262137 2.939634799092222], 1e-4);
asserts.isapprox(PCIM(:, :, 2), [2.255864510332375 0.8819362131118006;
                                6.524448640674929 3.455384638583897], 1e-4);
%=============================================================================
[PHAT, PCI] = wblfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [3.499523978509226 1.254115126547431], 1e-4);
%=============================================================================
[PHAT, PCI] = wblfit([1 NaN 3]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = wblfit([1 2 3], [], [1 1 1], []);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = wblfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('wblfit()', _('Not enough input arguments.'));
asserts.checkerror('wblfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('wblfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('wblfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('wblfit([0 1 2])', _('X must contain positive finite values.'));
asserts.checkerror('wblfit([1 Inf 2])', _('X must contain positive finite values.'));
asserts.checkerror('wblfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('wblfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('wblfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('wblfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('wblfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('wblfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('wblfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('wblfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('wblfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
