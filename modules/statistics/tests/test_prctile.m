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
asserts.isequal(nargin('prctile'), -1);
asserts.isequal(nargout('prctile'), 1);
%=============================================================================
C = (1:5)' * (2:6);
P = prctile(C, [25 50 75], 1);
REF = [3.5 5.25 7 8.75 10.5; ...
       6 9 12 15 18; ...
       8.5 12.75 17 21.25 25.5];
asserts.isapprox(P, REF, 1e-12);
P2 = prctile(C, [25 50 75], 2);
REF2 = [2.75 4 5.25; ...
        5.5 8 10.5; ...
        8.25 12 15.75; ...
        11 16 21; ...
        13.75 20 26.25];
asserts.isapprox(P2, REF2, 1e-12);
%=============================================================================
D = reshape(1:30, [3 5 2]);
Pall = prctile(D, [25 75], 'all');
asserts.isequal(Pall, [8; 23]);
Ppage = prctile(D, [25 75], [1 2]);
asserts.isapprox(Ppage(:), [4.25; 11.75; 19.25; 26.75], 1e-12);
%=============================================================================
methods = [prctile(1:5, 25, 'Method', 'midpoint'), ...
           prctile(1:5, 25, 'Method', 'inclusive'), ...
           prctile(1:5, 25, 'Method', 'exclusive')];
asserts.isapprox(methods, [1.75 2 1.5], 1e-12);
%=============================================================================
S = single([1 2 3]);
Psingle = prctile(S, [25 50 75]);
asserts.isequal(class(Psingle), 'single');
asserts.isapprox(double(Psingle), [1.25 2 2.75], 1e-6);
%=============================================================================
asserts.checkerror('prctile()', _('Not enough input arguments.'));
asserts.checkerror('prctile({1}, 50)', sprintf(_('%s must be a real numeric array.'), 'A'));
asserts.checkerror('prctile([1 2], -1)', _('Percentages must be in the range [0,100].'));
asserts.checkerror('prctile([1 2], 101)', _('Percentages must be in the range [0,100].'));
asserts.checkerror('prctile([1 2], 50, ''Method'', ''bad'')', _('Invalid quantile method.'));
%=============================================================================
