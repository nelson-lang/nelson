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
asserts.isequal(nargin('uiprogressdlg'), -2);
asserts.isequal(nargout('uiprogressdlg'), 1);
%=============================================================================
asserts.checkerror('uiprogressdlg()', _('Not enough input arguments.'));
%=============================================================================
f = figure('Visible', 'off');
d = uiprogressdlg(f, 'Title', 'Please wait', 'Message', 'Loading data', 'Value', 0.3);
%=============================================================================
% uiprogressdlg returns a graphics object of type 'ProgressDialog'.
asserts.istrue(isgraphics(d));
asserts.isequal(get(d, 'Type'), 'ProgressDialog');
asserts.isequal(class(d), 'ProgressDialog');
%=============================================================================
% Properties take the given values.
asserts.isequal(d.Value, 0.3);
asserts.isequal(d.Message, 'Loading data');
asserts.isequal(d.Title, 'Please wait');
asserts.isequal(char(d.Indeterminate), 'off');
asserts.isequal(d.Interpreter, 'none');
asserts.isequal(d.CancelText, 'Cancel');
%=============================================================================
% Properties are writable.
d.Value = 0.7;
asserts.isequal(d.Value, 0.7);
d.Message = 'Almost done';
asserts.isequal(d.Message, 'Almost done');
d.Indeterminate = 'on';
asserts.isequal(char(d.Indeterminate), 'on');
%=============================================================================
delete(d);
asserts.isfalse(isvalid(d));
%=============================================================================
close(f);
%=============================================================================
