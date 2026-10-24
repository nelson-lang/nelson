%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--WEB GUI REQUIRED--> run with: nelson-webview --file runtests_web.m
%=============================================================================
% The web engine must identify itself and create figures offscreen.
assert_isequal(getnelsonmode(), 'WEB_GUI');
assert_isequal(getwebmode(), 'webview');
%=============================================================================
f = figure();
assert_istrue(isgraphics(f));
plot(1:10);
close(f);
%=============================================================================
f = uifigure('Visible', 'off');
d = uiprogressdlg(f, 'Title', 'Import', 'Message', 'Reading data', 'Value', 0.25);
assert_isequal(get(d, 'windowTitle'), 'Import');
assert_isequal(get(d, 'labelText'), 'Reading data');
assert_istrue(get(d, 'value') == 25);
set(d, 'value', 60);
set(d, 'labelText', 'Writing data');
assert_istrue(get(d, 'value') == 60);
assert_isequal(get(d, 'labelText'), 'Writing data');
set(d, 'minimum', 0);
set(d, 'maximum', 0);
assert_istrue(get(d, 'minimum') == 0);
assert_istrue(get(d, 'maximum') == 0);
delete(d);
assert_isfalse(isvalid(d));
close(f);
%=============================================================================
