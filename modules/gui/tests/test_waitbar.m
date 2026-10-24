%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
asserts.isequal(nargin('waitbar'), -3);
asserts.isequal(nargout('waitbar'), 1);
%=============================================================================
h = waitbar(0.25, 'Processing');
asserts.istrue(isgraphics(h, 'figure'));
asserts.isequal(h.Name, '');
asserts.isequal(char(h.Resize), 'off');
asserts.isequal(h.Units, 'points');
asserts.isequal(h.UserData, []);
ax = findobj(h, 'Type', 'axes');
asserts.istrue(~isempty(ax));
asserts.isequal(char(ax.Visible), 'off');
asserts.isequal(ax.Units, 'points');
h2 = waitbar(0.75, h, 'Almost done');
asserts.isequal(h2, h);
asserts.isequal(h.UserData, []);
asserts.isequal(h.Name, 'Almost done');
close(h);
%=============================================================================
asserts.checkerror('waitbar()', _('Not enough input arguments.'));
%=============================================================================
