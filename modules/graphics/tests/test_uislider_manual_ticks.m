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
% <--GUI MODE-->
%=============================================================================
% Non-regression: setting MajorTicks (or MinorTicks) on a uislider must switch
% the matching *Mode to 'manual' so the automatic tick generation triggered by
% the Limits change (including the default one applied at creation) stops
% overwriting the user values. Previously 'MajorTicks',[5 40] was silently
% replaced by the auto ticks [0 20 40 60 80 100].
%=============================================================================
fig = uifigure('Visible', 'off');
%=============================================================================
% Manual MajorTicks given together with Limits survive creation.
sld = uislider(fig, 'Limits', [0 100], 'Value', 10, 'MajorTicks', [5 40]);
assert_isequal(sld.MajorTicks, [5 40]);
assert_isequal(sld.MajorTicksMode, 'manual');
assert_isequal(sld.Value, 10);
% auto labels follow the manual ticks
assert_isequal(sld.MajorTickLabels, {'5', '40'});
%=============================================================================
% Manual ticks persist across a later Limits change (auto would regenerate).
sld.Limits = [0 200];
assert_isequal(sld.MajorTicks, [5 40]);
assert_isequal(sld.MajorTicksMode, 'manual');
%=============================================================================
% MinorTicks assignment also flips its own mode to manual.
sld.MinorTicks = [10 20 30];
assert_isequal(sld.MinorTicks, [10 20 30]);
assert_isequal(sld.MinorTicksMode, 'manual');
%=============================================================================
% A slider left untouched keeps auto ticks that track the limits.
sldAuto = uislider(fig, 'Limits', [0 10]);
assert_isequal(sldAuto.MajorTicksMode, 'auto');
assert_isequal(sldAuto.MajorTicks, [0 2 4 6 8 10]);
sldAuto.Limits = [0 100];
assert_isequal(sldAuto.MajorTicks, [0 20 40 60 80 100]);
%=============================================================================
