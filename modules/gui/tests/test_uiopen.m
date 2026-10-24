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
asserts.isequal(nargin('uiopen'), 2);
asserts.isequal(nargout('uiopen'), 0);
%=============================================================================
asserts.checkerror('r = uiopen()', _('Too many output arguments.'));
asserts.checkerror('uiopen(1, 2, 3)', _('Too many input arguments.'));
%=============================================================================
% Unsupported model file formats fail fast (no file picker, no hang) when the
% caller asks to open the file directly with the second 'direct' argument.
asserts.checkerror('uiopen(''bandwidth_model.slx'', 1)', sprintf(_('''%s'' files are not supported.'), '.slx'), 'Nelson:uiopen:unsupportedFileType');
asserts.checkerror('uiopen(''model.mdl'', 1)', sprintf(_('''%s'' files are not supported.'), '.mdl'), 'Nelson:uiopen:unsupportedFileType');
% The check is case insensitive on the extension.
asserts.checkerror('uiopen(''BANDWIDTH_MODEL.SLX'', true)', sprintf(_('''%s'' files are not supported.'), '.slx'), 'Nelson:uiopen:unsupportedFileType');
%=============================================================================
% Direct open of an existing data file loads it without any dialog.
nh5_file = [tempdir(), 'test_uiopen_fixture.nh5'];
uiopen_fixture_value = 42;
save(nh5_file, 'uiopen_fixture_value');
clear('uiopen_fixture_value');
uiopen(nh5_file, 1);
asserts.isequal(uiopen_fixture_value, 42);
clear('uiopen_fixture_value');
% An existing file is opened directly even without the 'direct' flag.
uiopen(nh5_file);
asserts.isequal(uiopen_fixture_value, 42);
rmfile(nh5_file);
%=============================================================================
