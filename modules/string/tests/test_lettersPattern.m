%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(char(lettersPattern()), '(?:[A-Za-z])+');
asserts.isequal(extract("The 2 parties agreed.", lettersPattern), ["The"; "parties"; "agreed"]);
%=============================================================================
