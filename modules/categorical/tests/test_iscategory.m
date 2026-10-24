%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical(["shirt" "pants"; "pants" "hat"]);
asserts.isequal(iscategory(A, ["shirt" "socks" "hat"]), [true false true]);
asserts.istrue(iscategory(A, "pants"));
%=============================================================================
