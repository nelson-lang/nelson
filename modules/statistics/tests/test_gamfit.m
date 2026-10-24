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
asserts.isequal(nargin('gamfit'), 5);
asserts.isequal(nargout('gamfit'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[PHAT, PCI] = gamfit(x);
asserts.isapprox(PHAT, [1.457461695639626 2.229904229883514], 1e-4);
asserts.isapprox(PCI, [0.5214102891372250 0.6563191849867488;
                      4.073940692216924 7.576302793209956], 1e-4);
%=============================================================================
[PHAT, PCI] = gamfit(x, 0.01);
asserts.isapprox(PHAT, [1.457461695639626 2.229904229883514], 1e-4);
asserts.isapprox(PCI, [0.3774896385415579 0.4468986465987426;
                      5.627159999579379 11.12662325629513], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[PHAT, PCI] = gamfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, [1.164302008282069 3.543364968242789], 1e-4);
asserts.isapprox(PCI, [0.4336474462489769 0.8906883059168973;
                      3.126039777739972 14.09632888942595], 1e-4);
%=============================================================================
opts = statset('MaxIter', 120, 'TolX', 1e-4);
[PHAT2, PCI2] = gamfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, [1.457461695639626 2.229904229883514], 1e-4);
asserts.isapprox(PCI2, [0.5214102891372250 0.6563191849867488;
                       4.073940692216924 7.576302793209956], 1e-4);
%=============================================================================
xm = [0.5 1; 1 2; 2 3; 3 4; 5 7];
[PHATM, PCIM] = gamfit(xm);
asserts.isequal(size(PHATM), [2 2]);
asserts.isapprox(PHATM, [1.865843876219960 1.232686201301903;
                        2.667694439829146 1.274508785278180], 1e-4);
asserts.isequal(size(PCIM), [2 2 2]);
asserts.isapprox(PCIM(:, :, 1), [0.5931476836361220 0.3314592506943277;
                                5.869319676148717 4.584319996189861], 1e-4);
asserts.isapprox(PCIM(:, :, 2), [0.8276325028118584 0.3516878401019491;
                                8.598736274997556 4.618791037189059], 1e-4);
%=============================================================================
[PHAT, PCI] = gamfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [1.457461695639626 2.229904229883514], 1e-4);
%=============================================================================
[PHAT, PCI] = gamfit([1 NaN 3]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = gamfit([1 2 3], [], [1 1 1], []);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = gamfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('gamfit()', _('Not enough input arguments.'));
asserts.checkerror('gamfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('gamfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('gamfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gamfit([0 1 2])', _('X must contain positive finite values.'));
asserts.checkerror('gamfit([1 Inf 2])', _('X must contain positive finite values.'));
asserts.checkerror('gamfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('gamfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('gamfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('gamfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('gamfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('gamfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('gamfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('gamfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('gamfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
