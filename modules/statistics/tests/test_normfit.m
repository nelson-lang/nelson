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
asserts.isequal(nargin('normfit'), 5);
asserts.isequal(nargout('normfit'), 4);
%=============================================================================
x = [-2 -1 0 1 3 5];
[MU, SIGMA, MUCI, SIGMACI] = normfit(x);
asserts.isapprox(MU, 1, 1e-4);
asserts.isapprox(SIGMA, 2.380476142847617, 1e-4);
asserts.isapprox(MUCI, [-0.9047021266816342; 2.904702126681634], 1e-4);
asserts.isapprox(SIGMACI, [1.351897201546178; 4.191640207691565], 1e-4);
%=============================================================================
[MU, SIGMA, MUCI, SIGMACI] = normfit(x, 0.01);
asserts.isapprox(MU, 1, 1e-4);
asserts.isapprox(SIGMA, 2.380476142847617, 1e-4);
asserts.isapprox(MUCI, [-1.503202911450328; 3.503202911450328], 1e-4);
asserts.isapprox(SIGMACI, [1.131703860958749; 5.007199199502614], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[MU, SIGMA, MUCI, SIGMACI] = normfit(x, 0.05, censor, freq);
asserts.isapprox(MU, 1.470514831431195, 1e-4);
asserts.isapprox(SIGMA, 2.809793797777060, 1e-4);
asserts.isapprox(MUCI, [-0.5775372818646674; 3.518566944727057], 1e-4);
asserts.isapprox(SIGMACI, [1.554558896539061; 5.078573223313098], 1e-4);
%=============================================================================
opts = statset('MaxIter', 120, 'TolX', 1e-4);
[MU2, SIGMA2] = normfit(x, [], [], [], opts);
asserts.isapprox(MU2, 1, 1e-4);
asserts.isapprox(SIGMA2, 2.380476142847617, 1e-4);
%=============================================================================
xm = [-1 1; 0 2; 1 3; 2 4; 4 7];
[MUM, SIGMAM, MUCIM, SIGMACIM] = normfit(xm);
asserts.isequal(size(MUM), [1 2]);
asserts.isapprox(MUM, [1.2 3.4], 1e-4);
asserts.isapprox(SIGMAM, [1.720465053408525 2.059126028197400], 1e-4);
asserts.isequal(size(MUCIM), [2 2]);
asserts.isapprox(MUCIM, [-0.3080306530507109 1.595131239093860;
                        2.708030653050711 5.204868760906139], 1e-4);
asserts.isapprox(SIGMACIM, [0.9257043493315273 1.107922486625124;
                           3.197565186052636 3.826982529179987], 1e-4);
%=============================================================================
[MU, SIGMA] = normfit(single(x));
asserts.isequal(class(MU), 'double');
asserts.isequal(class(SIGMA), 'double');
asserts.isapprox(MU, 1, 1e-4);
asserts.isapprox(SIGMA, 2.380476142847617, 1e-4);
%=============================================================================
[MU, SIGMA, MUCI, SIGMACI] = normfit([1 NaN 3]);
asserts.istrue(isnan(MU));
asserts.istrue(isnan(SIGMA));
asserts.istrue(all(isnan(MUCI(:))));
asserts.istrue(all(isnan(SIGMACI(:))));
[MU, SIGMA, MUCI, SIGMACI] = normfit([1 2 3], [], [1 1 1], []);
asserts.istrue(isnan(MU));
asserts.istrue(isnan(SIGMA));
asserts.istrue(all(isnan(MUCI(:))));
asserts.istrue(all(isnan(SIGMACI(:))));
[MU, SIGMA, MUCI, SIGMACI] = normfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(isnan(MU));
asserts.istrue(isnan(SIGMA));
asserts.istrue(all(isnan(MUCI(:))));
asserts.istrue(all(isnan(SIGMACI(:))));
[MU, SIGMA, MUCI, SIGMACI] = normfit([2 2 2]);
asserts.isequal(MU, 2);
asserts.isequal(SIGMA, 0);
asserts.istrue(all(isnan(MUCI(:))));
asserts.isequal(SIGMACI, [0; 0]);
%=============================================================================
asserts.checkerror('normfit()', _('Not enough input arguments.'));
asserts.checkerror('normfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('normfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('normfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('normfit([1 Inf 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('normfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('normfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('normfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('normfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('normfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('normfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('normfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('normfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('normfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================