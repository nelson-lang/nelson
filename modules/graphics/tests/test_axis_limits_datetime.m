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
% xlim with datetime bounds on a datetime axis.
t = datetime(2024, 1, 1) + hours(0:10);
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, t, 1:11);
xlim(ax, [datetime(2024, 1, 1, 2, 0, 0), datetime(2024, 1, 1, 5, 0, 0)]);
asserts.isequal(get(ax, 'XLimMode'), 'manual');
xl = xlim(ax);
asserts.isequal(class(xl), 'datetime');
asserts.isequal(size(xl), [1 2]);
asserts.isequal(xl(1), datetime(2024, 1, 1, 2, 0, 0));
asserts.isapprox(datenum(xl(2)), datenum(datetime(2024, 1, 1, 5, 0, 0)), 1e-9);
asserts.isapprox(get(ax, 'XLim'), datenum([datetime(2024, 1, 1, 2, 0, 0), datetime(2024, 1, 1, 5, 0, 0)]), 1e-9);
%=============================================================================
% Current axes form and the mode keyword.
xlim([datetime(2024, 1, 1, 1, 0, 0), datetime(2024, 1, 1, 3, 0, 0)]);
xl = xlim();
asserts.isequal(class(xl), 'datetime');
asserts.isequal(xl(1), datetime(2024, 1, 1, 1, 0, 0));
asserts.isequal(xlim(ax, 'mode'), 'manual');
xlim(ax, 'auto');
asserts.isequal(get(ax, 'XLimMode'), 'auto');
asserts.isequal(class(xlim(ax)), 'datetime');
%=============================================================================
% Numeric limits are still accepted on a datetime axis.
xlim(ax, datenum([datetime(2024, 1, 1, 3, 0, 0), datetime(2024, 1, 1, 4, 0, 0)]));
asserts.isequal(xlim(ax, 'mode'), 'manual');
%=============================================================================
% Invalid datetime limits.
asserts.checkerror('xlim(ax, [datetime(2024, 1, 1, 5, 0, 0), datetime(2024, 1, 1, 2, 0, 0)]);', _('Limits must be two increasing datetime values.'));
asserts.checkerror('xlim(ax, [datetime(2024, 1, 1, 5, 0, 0), NaT]);', _('Limits must be two increasing datetime values.'));
asserts.checkerror('xlim(ax, datetime(2024, 1, 1, 5, 0, 0));', _('Limits must be two increasing datetime values.'));
delete(f);
%=============================================================================
% ylim with datetime bounds; a numeric axis rejects datetime limits.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:3, t(1:3));
ylim(ax, [datetime(2024, 1, 1, 0, 30, 0), datetime(2024, 1, 1, 1, 30, 0)]);
yl = ylim(ax);
asserts.isequal(class(yl), 'datetime');
asserts.isequal(yl(1), datetime(2024, 1, 1, 0, 30, 0));
asserts.isequal(class(xlim(ax)), 'double');
asserts.checkerror('xlim(ax, [datetime(2024, 1, 1), datetime(2024, 1, 2)]);', _('Datetime limits need an axis that displays datetime values.'));
asserts.checkerror('zlim(ax, [datetime(2024, 1, 1), datetime(2024, 1, 2)]);', _('Datetime limits need an axis that displays datetime values.'));
delete(f);
%=============================================================================
