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
close all
%=============================================================================
f = figure('Visible', 'off');
plot(1:10, (1:10) / 4);
ax = gca();
xlim([-1 11]);
xticks(0:2:10);
xtickformat('%.2f');
assert_isequal(xtickformat(), '%.2f');
assert_isequal(ax.XTickLabelFormat, '%.2f');
drawnow();
assert_isequal(ax.XTickLabel', {'0.00', '2.00', '4.00', '6.00', '8.00', '10.00'});
xtickformat('percentage');
drawnow();
assert_isequal(ax.XTickLabel', {'0%', '2%', '4%', '6%', '8%', '10%'});
xtickformat(ax, '%.1f');
drawnow();
assert_isequal(ax.XTickLabel', {'0.0', '2.0', '4.0', '6.0', '8.0', '10.0'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
plot([1000 1234567], [0 1]);
ax = gca();
xlim([0 1300000]);
xticks([1000 25000 1234567]);
xtickformat('usd');
drawnow();
assert_isequal(ax.XTickLabel', {'$1,000.00', '$25,000.00', '$1,234,567.00'});
close(f);
%=============================================================================
assert_checkerror('f = figure(''Visible'', ''off''); xtickformat(5); close(f);', getString(message('Nelson:validators:mustBeTextScalar', 'Format')));
%=============================================================================
close all
%=============================================================================
