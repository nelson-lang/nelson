%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
figure();
g = hggroup();
ax = gca();
assert_istrue(isempty(g.Children));
assert_isequal(g.DisplayName, '');
assert_isequal(g.Parent, ax);
assert_isequal(g.Tag, '');
assert_isequal(g.Type, 'hggroup');
assert_istrue(isempty(g.UserData));
assert_isequal(g.Visible, 'on');
%=============================================================================
figure();
g2 = hggroup();
ln = line(g2, randn(5),randn(5));
g2.Visible = 'off';
assert_isequal(g2.Visible, 'off');
g2.Visible = 'on';
assert_isequal(g2.Visible, 'on');
%=============================================================================
figure();
ax = gca();
g1 = hggroup(ax);
assert_isequal(g1.Parent, ax);
g2 = hggroup(g1);
assert_isequal(g2.Parent, g1);
g3 = hggroup(g2);
assert_isequal(g3.Parent, g2);
%=============================================================================
figure();
ax = gca();
g = hggroup();
h = text(0.1, 0.1, 'tttt', 'Parent', g);
assert_isequal(h.Parent, g);
%=============================================================================
assert_checkerror('g = hggroup(gcf());', _('Group can be only a child of axes or hggroup.'));
%============================================================================= 
figure();
hg = hggroup;
plot(hg, 1:2,1:2);
plot(hg, 4:5,4:5);
ax = gca();
assert_isequal(ax.XLim, [1 5]);
assert_isequal(ax.YLim, [1 5]);
%=============================================================================
