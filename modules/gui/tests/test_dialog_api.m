%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
% Desktop dialog builtins served through the web front end. These checks cover
% the signatures and every input-validation path that is resolved before any
% dialog is shown (so they run without a graphical front end).
%=============================================================================
% Signatures.
asserts.isequal(nargin('questdlg'), 6);
asserts.isequal(nargout('questdlg'), 1);
asserts.isequal(nargin('inputdlg'), 5);
asserts.isequal(nargout('inputdlg'), 1);
asserts.isequal(nargout('listdlg'), 2);
asserts.isequal(nargout('uigetfile'), 3);
asserts.isequal(nargout('uiputfile'), 3);
asserts.isequal(nargout('uigetdir'), 1);
asserts.isequal(nargout('uisetcolor'), 1);
asserts.isequal(nargout('uisetfont'), 1);
asserts.isequal(nargout('uialert'), 0);
asserts.isequal(nargout('uiconfirm'), 1);
%=============================================================================
% Too few / too many arguments.
asserts.checkerror('questdlg()', _('Not enough input arguments.'));
asserts.checkerror('[a, b] = questdlg(''q'', ''t'')', _('Too many output arguments.'));
asserts.checkerror('uialert(1)', _('Not enough input arguments.'));
asserts.checkerror('uiconfirm(1, 2)', _('Not enough input arguments.'));
%=============================================================================
% Type validation (raised before any front end is consulted).
asserts.checkerror('questdlg(5)', _('char, cell, or string expected.'));
asserts.checkerror('inputdlg(5)', _('''Text'' expected.'));
asserts.checkerror('listdlg(''Name'', ''untitled'')', _('''ListString'' expected.'));
%=============================================================================
% A non-figure first argument is rejected by the uifigure dialogs.
msg = _('Invalid or deleted figure handle. First argument must be a valid figure handle.');
asserts.checkerror('uialert(1, ''message'', ''title'')', msg);
asserts.checkerror('uiconfirm(1, ''message'', ''title'')', msg);
%=============================================================================
% Without a desktop front end the dialogs report that a display is required
% rather than blocking.
asserts.checkerror('questdlg(''ready?'')', ...
  _('questdlg requires a graphical desktop, which is not available.'));
asserts.checkerror('inputdlg(''name'')', ...
  _('inputdlg requires a graphical desktop, which is not available.'));
asserts.checkerror('uigetfile()', ...
  _('File dialogs require a graphical desktop, which is not available.'));
asserts.checkerror('uisetcolor()', ...
  _('uisetcolor requires a graphical desktop, which is not available.'));
%=============================================================================
