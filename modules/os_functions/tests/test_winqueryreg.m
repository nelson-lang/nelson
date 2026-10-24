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
asserts.isequal(nargin('winqueryreg'), -2);
asserts.isequal(nargout('winqueryreg'), 1);
%=============================================================================
r = winqueryreg('name', 'HKEY_CURRENT_USER', 'control panel\mouse');
asserts.istrue(iscellstr(r));
%=============================================================================
r = winqueryreg('HKEY_LOCAL_MACHINE', 'HARDWARE\DESCRIPTION\System\CentralProcessor\0\', 'ProcessorNameString')
asserts.istrue(ischar(r));
%=============================================================================
r = winqueryreg('HKEY_CLASSES_ROOT', 'Msxml');
asserts.isequal(r, 'Msxml');
%=============================================================================
r = winqueryreg('HKEY_CLASSES_ROOT', 'Msxml\clsid');
asserts.istrue(ischar(r));
asserts.isequal(length(r), 38);
%=============================================================================
asserts.checkerror('r = winqueryreg(''HKEY_CLASSES_ROO'', ''Msxml\clsid'')', _('Invalid ROOTKEY value.'));
asserts.checkerror('r = winqueryreg(''HKEY_CLASSES_ROOT'', ''Msxml\clsid33'')', _('Invalid SUBKEY value.'));
asserts.checkerror('r = winqueryreg(''HKEY_CURRENT_USER'',''control panel\mouse'')', _('Cannot convert RegQueryValueEx.'));
asserts.checkerror('r = winqueryreg(''name'', ''HKEY_CURRENT_USER'')', _("'name' argument requires 3 input arguments."));
%=============================================================================
