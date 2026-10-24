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
% The transpose operator "'" was mistaken for a char-array opening quote by the
% comment scanner, so a comment after a transpose (e.g. "[1 2 3]' % note") was
% kept in the captured default. The generated assignment then had its ";"
% pushed inside the comment, which both leaked a display during construction
% and left the value unterminated. This locks the corrected behavior: transpose
% then comment, escaped quote then comment, and a following property all parse.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
obj = ClassdefTransposeComment();
asserts.isequal(obj.A, [1; 2; 3]);
asserts.isequal(obj.S, 'it''s here');
asserts.isequal(obj.N, 5);
%=============================================================================
rmpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
