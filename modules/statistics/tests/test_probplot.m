%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
asserts.isequal(nargin('probplot'), -1);
asserts.isequal(nargout('probplot'), -1);
%=============================================================================
f = figure('Visible', 'off');
h = probplot([1 2 4 8]);
asserts.isequal(length(h), 2);
asserts.isequal(h(1).Marker, '+');
asserts.isequal(h(1).LineStyle, 'none');
asserts.isequal(h(1).XData, [1 2 4 8]);
asserts.isequal(h(1).YData, [0.125 0.375 0.625 0.875]);
asserts.isequal(h(2).LineStyle, '--');
asserts.isequal(h(2).XData([1 end]), [1 8]);
asserts.isapprox(h(2).YData([1 end]), [0.204862871038761 0.898674705485615], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = probplot('exponential', [1 2 4 8], 'noref');
asserts.isequal(length(h), 1);
asserts.isequal(h(1).XData, [1 2 4 8]);
asserts.isequal(h(1).YData, [0.125 0.375 0.625 0.875]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = probplot('logistic', [1 2 4 8]);
asserts.isequal(length(h), 2);
asserts.isapprox(h(2).YData([1 50 100]), ...
  [0.207058456256216 0.586360981122534 0.888464973607647], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = probplot([1 2 3; 4 NaN 6], [], [2 1 1; 1 0 2]);
asserts.isequal(length(h), 6);
asserts.isequal(h(1).XData, [1 1 4]);
asserts.isapprox(h(1).YData, [1 / 6 1 / 2 5 / 6], 1e-12);
asserts.isequal(h(3).XData, 2);
asserts.isequal(h(3).YData, 0.5);
asserts.isequal(h(5).XData, [3 6 6]);
asserts.isapprox(h(5).YData, [1 / 6 1 / 2 5 / 6], 1e-12);
close(f);
%=============================================================================
asserts.checkerror('probplot()', _('Not enough input arguments.'));
asserts.checkerror('probplot({1})', sprintf(_('%s must be a real numeric array.'), 'Y'));
asserts.checkerror('probplot(''bad'', [1 2])', sprintf(_('Unsupported %s.'), 'distribution'));
asserts.checkerror('probplot([1 2], [0 1 0])', sprintf(_('%s must match Y.'), 'Censoring'));
asserts.checkerror('probplot([1 2], [], [1; -1])', sprintf(_('%s must contain nonnegative integer values.'), 'Frequency'));
%=============================================================================
