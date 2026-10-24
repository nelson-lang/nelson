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
% Assumed-type failures carry distinct identifiers.
%=============================================================================
asserts.checkerror('bitshift(uint8(255), 4, ''uint16'')', 'Integer inputs cannot be assumed as any other types than their own types.', 'Nelson:operators:integerAssumedType');
%=============================================================================
