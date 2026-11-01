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
% No desktop here, so the Notebook editor cannot open; adv-cli loads text_editor for edit.
nlx = [tempdir(), 'test_notebook_edit_', createGUID(), '.nlx'];
msg = _('The Notebook editor requires the Nelson desktop.');
asserts.checkerror('notebook()', msg, 'Nelson:notebook:editorUnavailable');
cmd = sprintf('notebook(''%s'', ''edit'')', nlx);
asserts.checkerror(cmd, msg, 'Nelson:notebook:editorUnavailable');
%=============================================================================
% `edit` on a .nlx goes through the same door and reports the same error.
cmd = sprintf('edit(''%s'')', nlx);
asserts.checkerror(cmd, msg, 'Nelson:notebook:editorUnavailable');
%=============================================================================
% only 'edit' is accepted as second argument
cmd = sprintf('notebook(''%s'', ''run'')', nlx);
asserts.checkerror(cmd, _('Wrong value for #2 argument.'));
%=============================================================================
