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
msg = _('A ''persistent'' declaration is only allowed in a script file function.');
assert_checkerror('persistent bytecodePersistentBaseScope', msg);
assert_checkerror('persistent bytecodePersistentBaseA bytecodePersistentBaseB', msg);
%=============================================================================
