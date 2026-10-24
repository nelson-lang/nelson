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
asserts.isequal(nargin('uiprogressdlg'), -2);
asserts.isequal(nargout('uiprogressdlg'), 1);
%=============================================================================
asserts.checkerror('[a, b] = uiprogressdlg(1)', _('Too many output arguments.'));
asserts.checkerror('uiprogressdlg()', _('Not enough input arguments.'));
asserts.checkerror('uiprogressdlg(1)', _('Invalid or deleted figure handle. First argument must be a valid figure handle.'));
%=============================================================================
f = figure('Visible', 'off');
d = uiprogressdlg(f, 'Title', 'Import', 'Message', 'Reading data', 'Value', 0.25);
asserts.isequal(get(d, 'windowTitle'), 'Import');
asserts.isequal(get(d, 'labelText'), 'Reading data');
asserts.istrue(get(d, 'value') == 25);
set(d, 'value', 60);
set(d, 'labelText', 'Writing data');
asserts.istrue(get(d, 'value') == 60);
asserts.isequal(get(d, 'labelText'), 'Writing data');
set(d, 'minimum', 0);
set(d, 'maximum', 0);
asserts.istrue(get(d, 'minimum') == 0);
asserts.istrue(get(d, 'maximum') == 0);
delete(d);
asserts.isfalse(isvalid(d));
close(f);
%=============================================================================
