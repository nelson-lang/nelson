%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Context in which a subscripted reference or assignment is being evaluated.
% Passed by the interpreter as the third argument of numArgumentsFromSubscript
% so a class can report how many values an indexing operation produces:
%   Statement  - the indexing result is used as a whole statement
%   Expression - the indexing result is consumed as a comma-separated list
%   Assignment - the indexing appears on the left-hand side of an assignment
%=============================================================================
classdef IndexingContext
  enumeration
    Statement
    Expression
    Assignment
  end
end
%=============================================================================
