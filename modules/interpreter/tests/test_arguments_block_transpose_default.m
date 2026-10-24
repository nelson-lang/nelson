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
% The pre-lexer arguments rewrite mistook the transpose operator "'" for a
% char-array opening quote, so a comment after a transpose in a default (e.g.
% "[1 2 3]' % note") was swallowed into a string and the generated default was
% left unterminated: constructing leaked a display and the value was wrong.
% This locks the corrected behavior.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
asserts.isequal(arguments_edge_transpose_default(), 6);
asserts.isequal(arguments_edge_transpose_default([10 20]'), 30);
%=============================================================================
rmpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
