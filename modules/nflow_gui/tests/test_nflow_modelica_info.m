%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
info = jsondecode(NFlow.internal.modelicaInfoJson());
assert_istrue(isfield(info, 'available'));
assert_istrue(isfield(info, 'capable'));
assert_istrue(isfield(info, 'omc'));
assert_istrue(isfield(info, 'version'));
assert_istrue(isfield(info, 'reason'));
assert_istrue(islogical(info.available));
assert_istrue(islogical(info.capable));
%=============================================================================
