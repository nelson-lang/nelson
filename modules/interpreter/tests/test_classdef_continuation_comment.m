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
% A line continuation "..." may be followed by comment text; the structural
% scanner used to require "..." at the very end of the line, so a default such
% as "1 + ... comment" was not recognized as a continuation, the next line was
% dropped, and the malformed default made the whole class fail to register.
% This locks the corrected behavior on both a scalar default and a sized
% property whose size and default each span a commented continuation.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
obj = ClassdefContinuationCommentDefault();
asserts.isequal(obj.A, 3);
asserts.isequal(obj.B, [10 20]);
%=============================================================================
rmpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
