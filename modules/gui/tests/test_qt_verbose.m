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
% query form returns the current state as a logical scalar
initial = qt_verbose();
asserts.isequal(class(initial), 'logical');
asserts.istrue(isscalar(initial));
%=============================================================================
% set form returns the previous state and applies the new one
previous = qt_verbose(true);
asserts.isequal(class(previous), 'logical');
asserts.istrue(qt_verbose());
previous = qt_verbose(false);
asserts.istrue(previous);
asserts.isfalse(qt_verbose());
%=============================================================================
% toggling reports the value in effect before each change
asserts.isfalse(qt_verbose(true));
asserts.istrue(qt_verbose(false));
%=============================================================================
asserts.checkerror('qt_verbose(1, 2)', _('Too many input arguments.'));
asserts.checkerror('qt_verbose(''x'')', _('logical expected.'));
%=============================================================================
% leave the verbosity as it was found
qt_verbose(initial);
asserts.isequal(qt_verbose(), initial);
%=============================================================================
