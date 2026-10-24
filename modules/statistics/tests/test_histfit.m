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
% <--ADV-CLI MODE-->
%=============================================================================
asserts.isequal(nargin('histfit'), -1);
asserts.isequal(nargout('histfit'), -1);
%=============================================================================
f = figure('Visible', 'off');
h = histfit([1 2 2 3 4 5], 3);
asserts.isequal(length(h), 2);
asserts.isequal(h(1).Type, 'histogram');
asserts.isequal(h(1).BinCounts, [3 1 2]);
asserts.isequal(h(2).Type, 'line');
asserts.isequal(h(2).Color, [1 0 0]);
asserts.isapprox(h(2).XData([1 50 100]), [1 2.979797979797980 5], 1e-12);
asserts.isapprox(h(2).YData([1 50 100]), ...
  [0.936411382340370 2.361101668099504 0.647309228095023], 1e-12);
asserts.isapprox(h(2).UserData.Parameters, [2.833333333333333 1.343709624716425], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = histfit(ax, [1 2 3 4 5], [], 'exponential');
asserts.isequal(ancestor(h(1), 'axes'), ax);
asserts.isapprox(h(2).YData([1 50 100]), ...
  [1.592291801275087 0.823033563658763 0.419723561861248], 1e-12);
asserts.isequal(h(2).UserData.Distribution, 'exponential');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = histfit([0.2 0.3 0.4 0.6 0.8], 4, 'beta');
asserts.isapprox(h(2).UserData.Parameters, [2.002586206896551 2.350862068965517], 1e-12);
close(f);
%=============================================================================
for dist = {'kernel', 'gamma', 'extreme value', 'half normal', 'lognormal', ...
    'logistic', 'loglogistic', 'rayleigh', 'weibull'}
  f = figure('Visible', 'off');
  h = histfit([1 2 3 4 5 8 13], 4, dist{1});
  asserts.isequal(length(h), 2);
  asserts.isequal(h(2).UserData.Distribution, dist{1});
  close(f);
end
%=============================================================================
asserts.checkerror('histfit()', _('Not enough input arguments.'));
asserts.checkerror('histfit({1})', sprintf(_('%s must be a real numeric vector.'), 'Data'));
asserts.checkerror('histfit([1 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'Number of bins'));
asserts.checkerror('histfit([1 2], 2, ''bad'')', sprintf(_('Unsupported %s.'), 'distribution'));
asserts.checkerror('histfit([-1 2], 2, ''lognormal'')', _('Data must be positive for this distribution.'));
asserts.checkerror('histfit([0 1], 2, ''beta'')', _('Data must be in the interval (0,1) for this distribution.'));
%=============================================================================
