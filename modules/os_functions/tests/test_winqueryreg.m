%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
%=============================================================================
assert_isequal(nargin('winqueryreg'), -2);
assert_isequal(nargout('winqueryreg'), 1);
%=============================================================================
r = winqueryreg('name','HKEY_CURRENT_USER','control panel\mouse');
assert_istrue(iscellstr(r));
%=============================================================================
r = winqueryreg('HKEY_LOCAL_MACHINE', 'HARDWARE\DESCRIPTION\System\CentralProcessor\0\', 'ProcessorNameString')
assert_istrue(ischar(r));
%=============================================================================
r = winqueryreg('HKEY_CLASSES_ROOT', 'Msxml');
assert_isequal(r, 'Msxml');
%=============================================================================
r = winqueryreg('HKEY_CLASSES_ROOT', 'Msxml\clsid');
assert_istrue(ischar(r));
assert_isequal(length(r), 38);
%=============================================================================
assert_checkerror('r = winqueryreg(''HKEY_CLASSES_ROO'', ''Msxml\clsid'')', _('Invalid ROOTKEY value.'));
assert_checkerror('r = winqueryreg(''HKEY_CLASSES_ROOT'', ''Msxml\clsid33'')', _('Invalid SUBKEY value.'));
assert_checkerror('r = winqueryreg(''HKEY_CURRENT_USER'',''control panel\mouse'')', _('Cannot convert RegQueryValueEx.'));
assert_checkerror('r = winqueryreg(''name'', ''HKEY_CURRENT_USER'')', _("'name' argument requires 3 input arguments."));
%=============================================================================
