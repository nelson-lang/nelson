%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/116
% <-- Short Description -->
% fix display size of big sparse matrix
%=============================================================================
% <--CHECK REF-->
% <--ENGLISH IMPOSED-->
% <--INDEX 64 BIT REQUIRED-->
%=============================================================================
a = sparse(2e13, 2)
b = logical(a)
c = logical(a(1))
d = sparse(true)