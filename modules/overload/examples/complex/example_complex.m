%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
previousPath = addpath([modulepath('overload'), '/examples/complex']);
cleanupPath = onCleanup(@() path(previousPath));
c = complexObj(3, 4);
c(23, 44)
c
c + c
clear cleanupPath;
%=============================================================================
