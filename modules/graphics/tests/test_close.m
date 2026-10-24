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
asserts.isfalse(isempty(g.Children));
R = close('all');
asserts.istrue(R);
g = groot();
asserts.istrue(isempty(g.Children));
%=============================================================================
R = close('all');
asserts.istrue(R);
%=============================================================================
R = close();
asserts.istrue(R);
%=============================================================================
R = close([]);
asserts.istrue(R);
%=============================================================================
asserts.checkerror('close(''toto'')', getString(message('Nelson:close:WindowNotFound')))
%=============================================================================
f = figure('Name', 'NELSON');
asserts.checkerror('R = close(''Nelson'')', getString(message('Nelson:close:WindowNotFound')))
%=============================================================================
R = close('NELSON');
asserts.istrue(R);
%=============================================================================
figure();
figure();
figure();
R = close([1 3 2 4]);
asserts.istrue(R);
g = groot();
asserts.istrue(isempty(g.Children));
%=============================================================================
f1 = figure('Name', 'NELSON_1');
f2 = figure('Name', 'NELSON_2');
f3 = figure('Name', 'NELSON_3');
R = close('NELSON_3', 'NELSON_2', 'NELSON_4', 'NELSON_1');
asserts.istrue(R);
%=============================================================================
% 'force' flag: close('all', 'force') must actually close every figure.
figure();
surf(peaks(10));
figure();
assert_isequal(numel(findobj(0, 'type', 'figure')), 2);
close('all', 'force');
assert_isequal(numel(findobj(0, 'type', 'figure')), 0);
%=============================================================================
% 'force' flag with subplots.
figure();
subplot(2, 2, 1);
subplot(2, 2, 2);
subplot(2, 2, 3);
subplot(2, 2, 4);
figure();
subplot(1, 2, 1);
subplot(1, 2, 2);
assert_isequal(numel(findobj(0, 'type', 'figure')), 2);
close('all', 'force');
assert_isequal(numel(findobj(0, 'type', 'figure')), 0);
%=============================================================================
% close(ID, 'force').
f = figure();
R = close(f, 'force');
asserts.istrue(R);
assert_isequal(numel(findobj(0, 'type', 'figure')), 0);
%=============================================================================
% close(name, 'force').
figure('Name', 'FORCE_ME');
R = close('FORCE_ME', 'force');
asserts.istrue(R);
assert_isequal(numel(findobj(0, 'type', 'figure')), 0);
%=============================================================================
% close('force') force-closes the current figure.
figure();
close('force');
assert_isequal(numel(findobj(0, 'type', 'figure')), 0);
%=============================================================================
% 'force' bypasses a vetoing CloseRequestFcn, plain close does not.
f = figure('CloseRequestFcn', '1;');
assert_isequal(numel(findobj(0, 'type', 'figure')), 1);
close('all');
assert_isequal(numel(findobj(0, 'type', 'figure')), 1);
close('all', 'force');
assert_isequal(numel(findobj(0, 'type', 'figure')), 0);
%=============================================================================
