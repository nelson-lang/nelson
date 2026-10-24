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
% A discrete knob has no Limits property: exporting a uifigure that holds one
% (the generic control serialization behind saveas / the web renderer) must
% bound its dial on the Items and not look the property up.
fig = uifigure('Visible', 'off', 'Name', 'Knobs', 'Position', [100 100 620 360]);
kb = uiknob(fig);
kb.Position = [70 90 170 170];
kb.Value = 55;
dk = uiknob(fig, 'discrete');
dk.Position = [310 70 260 220];
dk.Items = {'Low', 'Medium', 'High'};
dk.Value = 'Medium';
drawnow();
assert_isequal(dk.ValueIndex, 2);
assert_isfalse(isprop(dk, 'Limits'));

svgName = [tempname(), '.svg'];
saveas(fig, svgName);
assert_istrue(isfile(svgName));
svg = fileread(svgName);
assert_istrue(~isempty(strfind(svg, '<svg')));
rmfile(svgName);

pngName = [tempname(), '.png'];
saveas(fig, pngName);
assert_istrue(isfile(pngName));
img = imread(pngName);
assert_istrue(size(img, 1) > 100 && size(img, 2) > 100);
rmfile(pngName);

delete(fig);
