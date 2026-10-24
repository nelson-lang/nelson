%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = timetableVariableNames(TT, vars)
  % Names of the variables of a timetable (or event table) selected by
  % vars: names, indices or logical mask, resolved by indexing so that the
  % errors are those of TT(:, vars).
  sub = subsref(TT, struct('type', '()', 'subs', {{[], vars}}));
  props = subsref(sub, struct('type', '.', 'subs', 'Properties'));
  names = props.VariableNames;
end
%=============================================================================
