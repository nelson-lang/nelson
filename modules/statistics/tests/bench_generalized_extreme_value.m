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
x = linspace(-4, 8, 1000000);
tic(); y = gevpdf(x, 0.2, 1, 0); t1 = toc();
tic(); p = gevcdf(x, 0.2, 1, 0); t2 = toc();
tic(); q = gevinv(p, 0.2, 1, 0); t3 = toc();
asserts.istrue(all(isfinite(y(:))));
asserts.istrue(all(isfinite(p(:))));
asserts.istrue(all(isfinite(q(:))));
asserts.istrue(t1 + t2 + t3 < 10);
%=============================================================================
