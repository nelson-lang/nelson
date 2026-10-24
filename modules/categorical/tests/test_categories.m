%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical(["plane" "car" "train" "car"]);
asserts.isequal(categories(A), {'car'; 'plane'; 'train'});
S = categories(A, OutputType= "string");
asserts.isequal(cellstr(S), {'car'; 'plane'; 'train'});
%=============================================================================
