%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(isfolder(nelsonroot()));
asserts.isfalse(isfolder('blabla_isfolder'));
asserts.isequal(isfolder(nelsonroot()), isdir(nelsonroot()));
%=============================================================================