%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Map identifiers to values and retrieve selected entries.
codes = ["FR", "DE", "ES"];
capitals = ["Paris", "Berlin", "Madrid"];
capitalByCode = dictionary(codes, capitals);
disp("Capital for DE: " + capitalByCode("DE"));
