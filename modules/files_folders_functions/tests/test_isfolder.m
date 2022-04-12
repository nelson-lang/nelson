%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(isfolder(nelsonroot()));
assert_isfalse(isfolder('blabla_isfolder'));
assert_isequal(isfolder(nelsonroot()), isdir(nelsonroot()));
%=============================================================================