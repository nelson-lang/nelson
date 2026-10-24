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
asserts.isequal(nargin('nbinfit'), 5);
asserts.isequal(nargout('nbinfit'), 2);
%=============================================================================
x = [0 1 2 4 6 9 12 15];
[PHAT, PCI] = nbinfit(x);
asserts.isapprox(PHAT, [1.271927087364485 0.1719534400291764], 1e-4);
asserts.isapprox(PCI, [0.3562699906252136 0;
                      4.540934005506463 0.3770786298834723], 1e-4);
%=============================================================================
[PHAT, PCI] = nbinfit(x, 0.01);
asserts.isapprox(PHAT, [1.271927087364485 0.1719534400291764], 1e-4);
asserts.isapprox(PCI, [0.2388436313154222 0;
                      6.773463067286063 0.4415336359360506], 1e-4);
%=============================================================================
censor = [0 0 0 0 1 0 1 0];
freq = [1 2 1 1 1 2 1 1];
[PHAT, PCI] = nbinfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, [0.9261527213892123 0.1071618595625095], 1e-4);
asserts.isapprox(PCI, [0.3020295791534707 0;
                      2.839982976967927 0.2514775346446225], 1e-4);
%=============================================================================
opts = statset('MaxIter', 160, 'TolX', 1e-4);
[PHAT2, PCI2] = nbinfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, [1.271927087364485 0.1719534400291764], 1e-4);
asserts.isapprox(PCI2, [0.3562699906252136 0;
                       4.540934005506463 0.3770786298834723], 1e-4);
%=============================================================================
xm = [0 1; 1 2; 2 4; 4 6; 6 9; 9 13];
[PHATM, PCIM] = nbinfit(xm);
asserts.isequal(size(PHATM), [2 2]);
asserts.isapprox(PHATM, [1.628798431072445 0.3075836409696574;
                        2.633920446704113 0.3110713936976822], 1e-4);
asserts.isequal(size(PCIM), [2 2 2]);
asserts.isapprox(PCIM(:, :, 1), [0.2603876391517490 0;
                                10.18859550210042 0.7297437717874288], 1e-4);
asserts.isapprox(PCIM(:, :, 2), [0.4901873818192576 0;
                                14.15282640246339 0.6932373419469122], 5e-4);
%=============================================================================
[PHAT, PCI] = nbinfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [1.271927087364485 0.1719534400291764], 1e-4);
%=============================================================================
[PHAT, PCI] = nbinfit([1 NaN 3]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = nbinfit([0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = nbinfit([1 2 3], [], [1 1 1], []);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = nbinfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('nbinfit()', _('Not enough input arguments.'));
asserts.checkerror('nbinfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('nbinfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('nbinfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('nbinfit([-1 1 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('nbinfit([1.5 2 3])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('nbinfit([1 Inf 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('nbinfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('nbinfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('nbinfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('nbinfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('nbinfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('nbinfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('nbinfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('nbinfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('nbinfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
