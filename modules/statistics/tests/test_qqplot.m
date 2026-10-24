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
asserts.isequal(nargin('qqplot'), -1);
asserts.isequal(nargout('qqplot'), -1);
%=============================================================================
f = figure('Visible', 'off');
h = qqplot([1 2 4 8]);
asserts.isequal(length(h), 3);
asserts.isequal(h(1).Type, 'line');
asserts.isequal(h(1).Marker, '+');
asserts.isequal(h(1).LineStyle, 'none');
asserts.isapprox(h(1).XData, [-1.150349380376008 -0.318639363964375 ...
  0.318639363964375 1.150349380376008], 1e-12);
asserts.isequal(h(1).YData, [1 2 4 8]);
asserts.isapprox(h(2).XData, [-0.674489750196082 0.674489750196082], 1e-12);
asserts.isequal(h(2).YData, [1.5 6]);
asserts.isapprox(h(3).YData, [-0.087398722654531 7.587398722654530], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = qqplot([1 2 4 8], [2 3 7 11], [25 50 75]);
asserts.isequal(length(h), 3);
asserts.isequal(h(1).XData, [1.5 3 6]);
asserts.isequal(h(1).YData, [2.5 5 9]);
asserts.isequal(h(2).XData, [1.5 6]);
asserts.isequal(h(2).YData, [2.5 9]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = qqplot([1 2; 3 NaN; 5 6], [2 4; 4 8; 10 12]);
asserts.isequal(length(h), 6);
asserts.isequal(h(1).XData, [1 3 5]);
asserts.isequal(h(1).YData, [2 4 10]);
asserts.isequal(h(4).XData, [2 6]);
asserts.isequal(h(4).YData, [5 11]);
close(f);
%=============================================================================
asserts.checkerror('qqplot()', _('Not enough input arguments.'));
asserts.checkerror('qqplot({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('qqplot([1 2], {1})', sprintf(_('%s must be a real numeric array.'), 'Y'));
asserts.checkerror('qqplot([1 2], [1 2], [-1 50])', _('P must contain values between 0 and 100.'));
asserts.checkerror('qqplot([1 2; 3 4], [1 2 3; 4 5 6])', sprintf(_('X and Y must have %s.'), 'compatible numbers of columns'));
%=============================================================================
