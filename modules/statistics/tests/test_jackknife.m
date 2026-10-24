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
asserts.isequal(nargin('jackknife'), -1);
asserts.isequal(nargout('jackknife'), 1);
%=============================================================================
x = (1:5)';
jackstat = jackknife(@mean, x);
asserts.isequal(size(jackstat), [5 1]);
asserts.isapprox(jackstat, [3.5; 3.25; 3; 2.75; 2.5], 1e-12);
%=============================================================================
jackstat = jackknife(@jackknifeTwoStats, x);
asserts.isequal(size(jackstat), [5 2]);
%=============================================================================
jackstat = jackknife(@jackknifeMeanSum, x, x + 1);
asserts.isequal(size(jackstat), [5 1]);
asserts.isapprox(jackstat, [8; 7.5; 7; 6.5; 6], 1e-12);
%=============================================================================
opts = statset('bootstrp');
jackstat = jackknife(@mean, x, 'Options', opts);
asserts.isequal(size(jackstat), [5 1]);
%=============================================================================
asserts.checkerror('jackknife()', _('Not enough input arguments.'));
asserts.checkerror('jackknife(''mean'', x)', sprintf(_('%s must be a function handle.'), 'JACKFUN'));
asserts.checkerror('jackknife(@mean)', _('Not enough input arguments.'));
asserts.checkerror('jackknife(@mean, 1)', sprintf(_('At least one %s is required.'), 'nonscalar data argument'));
asserts.checkerror('jackknife(@mean, x, ''Options'')', _('Name-value arguments must be paired.'));
asserts.checkerror('jackknife(@mean, x, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('jackknife(@mean, x, ''Options'', 1)', _('Options must be a structure.'));
asserts.checkerror('jackknife(@jackknifeMeanSum, x, [1; 2])', sprintf(_('%s must have the same number of rows.'), 'Nonscalar data arguments'));
%=============================================================================
function y = jackknifeTwoStats(x)
  y = [mean(x) std(x)];
end
%=============================================================================
function y = jackknifeMeanSum(a, b)
  y = mean(a + b);
end
%=============================================================================
