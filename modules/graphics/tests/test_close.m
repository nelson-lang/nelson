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
figure();
figure();
figure();
g = groot();
assert_isfalse(isempty(g.Children));
R = close('all');
assert_istrue(R);
g = groot();
assert_istrue(isempty(g.Children));
%=============================================================================
R = close('all');
assert_istrue(R);
%=============================================================================
R = close();
assert_istrue(R);
%=============================================================================
R = close([]);
assert_istrue(R);
%=============================================================================
assert_checkerror('close(''toto'')', _('Specified window does not exist.'))
%=============================================================================
f = figure('Name', 'NELSON');
assert_checkerror('R = close(''Nelson'')', _('Specified window does not exist.'))
%=============================================================================
R = close('NELSON');
assert_istrue(R);
%=============================================================================
figure();
figure();
figure();
R = close([1 3 2 4]);
assert_istrue(R);
g = groot();
assert_istrue(isempty(g.Children));
%=============================================================================
f1 = figure('Name', 'NELSON_1');
f2 = figure('Name', 'NELSON_2');
f3 = figure('Name', 'NELSON_3');
R = close('NELSON_3', 'NELSON_2', 'NELSON_4', 'NELSON_1');
assert_istrue(R);
%=============================================================================
