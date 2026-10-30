%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
salesFigureVisible = 'off';
salesAnimationEnabled = true;
salesFramePause = 0;
salesAnimationContinuous = false;
salesMonths = 24;
run([modulepath('spreadsheet', 'root'), '/examples/sales_race.m']);

assert_istrue(isgraphics(salesFigure));
% The sales history was written to and read back from a CSV file.
assert_istrue(isfile(salesFile));
assert_isequal(size(salesData), [salesMonths salesProductCount]);
% Cumulative sales never decrease over the months.
assert_istrue(all(all(diff(salesData, 1, 1) >= -1e-9)));
delete(salesFigure);
drawnow();
%=============================================================================
