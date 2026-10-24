%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
txt = "This is line one." + newline + "Here is line two.";
asserts.isequal(extract(txt, lineBoundary + lettersPattern), ["This"; "Here"]);
asserts.istrue(isa(lineBoundary(), 'pattern'));
%=============================================================================
