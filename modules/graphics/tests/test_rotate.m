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
% 90 degree rotation about z maps the +x direction onto +y
figure();
h = line([0 1], [0 0], [0 0]);
rotate(h, [0 0 1], 90, [0 0 0]);
xd = get(h, 'XData');
yd = get(h, 'YData');
assert_istrue(abs(xd(2)) < 1e-9);
assert_istrue(abs(yd(2) - 1) < 1e-9);
%=============================================================================
% azimuth/elevation direction form about z (elevation 90 deg = +z axis)
figure();
h = line([0 1], [0 0], [0 0]);
rotate(h, [0 90], 90, [0 0 0]);
assert_istrue(abs(get(h, 'XData')(2)) < 1e-9);
assert_istrue(abs(get(h, 'YData')(2) - 1) < 1e-9);
%=============================================================================
% a surface (vector XData/YData, matrix ZData) rotates without error
figure();
s = surf(peaks(10));
rotate(s, [1 0 0], 30);
assert_isequal(size(get(s, 'ZData')), [10 10]);
%=============================================================================
% errors
figure();
h = line([0 1], [0 0]);
assert_checkerror('rotate(h, [1 0 0 0], 10)', _('DIRECTION must be a two- or three-element vector.'));
assert_checkerror('rotate(h, [0 0 0], 10)', _('DIRECTION must be nonzero.'));
%=============================================================================
