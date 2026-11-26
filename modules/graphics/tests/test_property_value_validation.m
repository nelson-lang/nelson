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
% Numeric, fixed-size and text graphics properties reject values of the wrong
% type, size, sign or NaN, and keep accepting valid ones (integer types, Inf
% limits, any text).
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
l = line(ax, 1:3, 1:3);
t = text(ax, 0.5, 0.5, 'hi');
e = errorbar(axes(f), 1:3, 1:3, [0.1 0.1 0.1]);
%=============================================================================
% fixed-size vectors
common = {'zzz', NaN, {0, 1}, 1i, struct('a', 1), @sin, []};
bad2 = [common, {[0 1 2], [NaN 1]}];
for k = 1:numel(bad2)
  assert_istrue(setFails(ax, 'XLim', bad2{k}));
  assert_istrue(setFails(ax, 'TickLength', bad2{k}));
end
assert_istrue(setFails(ax, 'XLim', [1 0]));
bad3 = [common, {[0 1], [NaN 1 2]}];
for k = 1:numel(bad3)
  assert_istrue(setFails(ax, 'CameraPosition', bad3{k}));
end
assert_isfalse(setFails(ax, 'XLim', [0 Inf]));
assert_isfalse(setFails(ax, 'XLim', int8([0 5])));
assert_isfalse(setFails(ax, 'CameraPosition', [1 2 3]));
assert_isfalse(setFails(ax, 'TickLength', [0.02 0.05]));
%=============================================================================
% positive scalars
bad = {-1, 0, NaN, 'zzz', '', [], {1}, 1i, 1:3, struct('a', 1)};
for k = 1:numel(bad)
  assert_istrue(setFails(l, 'LineWidth', bad{k}));
  assert_istrue(setFails(l, 'MarkerSize', bad{k}));
  assert_istrue(setFails(t, 'FontSize', bad{k}));
end
assert_isfalse(setFails(l, 'LineWidth', 2));
assert_isfalse(setFails(l, 'LineWidth', int8(5)));
assert_isfalse(setFails(l, 'MarkerSize', 1e308));
assert_isfalse(setFails(t, 'FontSize', 14));
%=============================================================================
% text properties
bad = {5, -1, {1, 2}, 1i, struct('a', 1), @sin, ones(3, 3, 3)};
for k = 1:numel(bad)
  assert_istrue(setFails(l, 'DisplayName', bad{k}));
  assert_istrue(setFails(t, 'FontName', bad{k}));
end
assert_isfalse(setFails(l, 'DisplayName', 'curve'));
assert_isfalse(setFails(l, 'DisplayName', ''));
assert_isfalse(setFails(t, 'FontName', 'Courier New'));
%=============================================================================
% variable-length numeric data
bad = {'zzz', {1, 2}, 1i, struct('a', 1), @sin};
for k = 1:numel(bad)
  assert_istrue(setFails(e, 'YNegativeDelta', bad{k}));
end
assert_isfalse(setFails(e, 'YNegativeDelta', [0.2 NaN 0.2]));
delete(f);
%=============================================================================
function failed = setFails(h, name, value)
  failed = false;
  try
    set(h, name, value);
  catch
    failed = true;
  end
end
%=============================================================================
